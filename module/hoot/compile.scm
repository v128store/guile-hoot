;;; WebAssembly linker
;;; Copyright (C) 2023 Igalia, S.L.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Linker for WebAssembly, to augment a wasm module by pulling in
;;; missing definitions from a standard library.
;;;
;;; Code:

(define-module (hoot compile)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 control)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module (system base compile)
  #:use-module (system base language)
  #:use-module (language cps)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:use-module (language cps tailify)
  #:use-module (language cps verify)
  #:use-module (language cps renumber)
  #:use-module (language cps dce)
  #:use-module (language cps simplify)
  #:use-module (language cps dump)
  #:use-module (language cps utils)
  #:use-module (wasm assemble)
  #:use-module (wasm dump)
  #:use-module (wasm link)
  #:use-module (wasm resolve)
  #:use-module (wasm parse)
  #:use-module (wasm types)
  #:export (compile-to-wasm))

(define (invert-tree parents)
  (intmap-fold
   (lambda (child parent tree)
     (let ((tree (intmap-add tree child empty-intset intset-union)))
       (match parent
         (-1 tree)
         (_ (intmap-add tree parent (intset child) intset-union)))))
   parents empty-intmap))

(define (intset-filter pred set)
  (persistent-intset
   (intset-fold (lambda (i out)
                  (if (pred i) (intset-add! out i) out))
                set empty-intset)))

(define (intset-pop set)
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define (intmap-map->list f map)
  (intmap-fold-right (lambda (k v out) (cons (f k v) out)) map '()))

(define void-block-type (make-type-use #f (make-func-sig '() '())))
(define i32-block-type (make-type-use #f (make-func-sig '() '(i32))))

;; Codegen improvements:
;;
;; 1. Eliminating directly-used locals.  Compute a set of variables that
;;    are used once, right after they are defined, in such a way that
;;    they can just flow to their use sites on the stack.  Avoid
;;    creating locals for these.
;;
;; 2. Instruction selection.  Could be that some instructions could be
;;    combined or rescheduled.  Perhaps this is something on the CPS
;;    level though.
;;
;; 3. Optimised local allocation.  Do graph coloring to map variables to
;;    a smaller set of locals, to avoid emitting one local per variable.

(define scm-type (make-ref-type #f 'eq))
(define kvarargs-sig
  (make-func-sig (list (make-param '$nargs 'i32)
                       (make-param '$arg0 scm-type)
                       (make-param '$arg1 scm-type)
                       (make-param '$arg2 scm-type))'()))

(define (export-abi wasm)
  (define abi-exports
    (list (make-export "$arg3" 'global '$arg3)
          (make-export "$arg4" 'global '$arg4)
          (make-export "$arg5" 'global '$arg5)
          (make-export "$arg6" 'global '$arg6)
          (make-export "$arg7" 'global '$arg7)
          (make-export "$argv" 'table '$argv)
          (make-export "$return-sp" 'global '$return-sp)
          (make-export "$return-stack" 'table '$return-stack)
          (make-export "$string->symbol" 'func '$string->symbol)
          (make-export "$symbol->keyword" 'func '$symbol->keyword)))
  (define (add-export export exports)
    (cons export exports))
  (match wasm
    (($ <wasm> types imports funcs tables memories globals exports
        start elems datas tags strings custom)
     (make-wasm types imports funcs tables memories globals
                (reverse (fold1 add-export abi-exports (reverse exports)))
                start elems datas tags strings custom))))

(define (compute-stdlib import-abi?)
  (define func-types
    (list (make-type '$kvarargs kvarargs-sig)))

  (define heap-types
    (letrec-syntax
        ((struct-field (syntax-rules (mut)
                         ((_ name (mut type))
                          (make-field 'name #t type))
                         ((_ name type)
                          (make-field 'name #f type))))
         (struct-type (syntax-rules ()
                        ((_ (field spec ...) ...)
                         (make-struct-type (list (struct-field field spec ...) ...)))))
         (struct* (syntax-rules ()
                    ((_ (name) (field spec ...) ...)
                     (make-type 'name
                                (struct-type (field spec ...) ...)))
                    ((_ (name super ...) (field spec ...) ...)
                     (make-type 'name
                                (make-sub-type
                                 '(super ...)
                                 (struct-type (field spec ...) ...))))))
         (struct (syntax-rules ()
                   ((_ (name super ...) (field spec ...) ...)
                    (struct* (name super ...)
                             ($hash (mut 'i32))
                             (field spec ...) ...)))))
      (list
       (make-type '$raw-bitvector (make-array-type #t 'i32))
       (make-type '$raw-bytevector (make-array-type #t 'i8))
       (make-type '$raw-scmvector (make-array-type #t scm-type))
       ;; In theory we could just include those members of the rec group
       ;; that the program needs, but to allow interoperability with
       ;; separately-compiled modules, we'll just put in the whole rec
       ;; group if any member is needed.
       (make-rec-group
        (list
         (struct ($heap-object))
         (struct ($extern-ref $heap-object)
                 ($val (make-ref-type #f 'extern)))
         (struct ($bignum $heap-object)
                 ($val (make-ref-type #f 'extern)))
         (struct ($flonum $heap-object)
                 ($val 'f64))
         (struct ($complex $heap-object)
                 ($real 'f64)
                 ($imag 'f64))
         (struct ($fraction $heap-object)
                 ($num scm-type)
                 ($denom scm-type))
         (struct ($pair $heap-object)
                 ($car (mut scm-type))
                 ($cdr (mut scm-type)))
         (struct ($mutable-pair $pair)
                 ($car (mut scm-type))
                 ($cdr (mut scm-type)))
         (struct ($vector $heap-object)
                 ($vals (make-ref-type #f '$raw-scmvector)))
         (struct ($mutable-vector $vector)
                 ($vals (make-ref-type #f '$raw-scmvector)))
         (struct ($bytevector $heap-object)
                 ($vals (make-ref-type #f '$raw-bytevector)))
         (struct ($mutable-bytevector $bytevector)
                 ($vals (make-ref-type #f '$raw-bytevector)))
         (struct ($bitvector $heap-object)
                 ($len 'i32)
                 ($vals (make-ref-type #f '$raw-bitvector)))
         (struct ($mutable-bitvector $bitvector)
                 ($len 'i32)
                 ($vals (make-ref-type #f '$raw-bitvector)))
         (struct ($string $heap-object)
                 ($str (mut (make-ref-type #f 'string))))
         (struct ($mutable-string $string)
                 ($str (mut (make-ref-type #f 'string))))
         (struct ($proc $heap-object)
                 ($func (make-ref-type #f '$kvarargs)))
         (struct ($symbol $heap-object)
                 ($name (make-ref-type #f '$string)))
         (struct ($keyword $heap-object)
                 ($name (make-ref-type #f '$symbol)))
         (struct ($variable $heap-object)
                 ($val (mut scm-type)))
         (struct ($atomic-box $heap-object)
                 ($val (mut scm-type)))
         (struct ($hash-table $heap-object)
                 ($size (mut (make-ref-type #f 'i31)))
                 ($buckets (make-ref-type #f '$vector)))
         (struct ($weak-table $heap-object)
                 ($val (make-ref-type #f 'extern)))
         (struct ($fluid $heap-object)
                 ($init scm-type))
         (struct ($dynamic-state $heap-object)
                 ($val (make-ref-type #f 'extern)))
         (struct ($syntax $heap-object)
                 ($expr scm-type)
                 ($wrap scm-type)
                 ($module scm-type)
                 ($source scm-type))
         (struct* ($port-type)
                  ($name (make-ref-type #f 'string))
                  ;; in guile these are (port, bv, start, count) -> size_t
                  ($read (make-ref-type #t '$proc)) ;; could have a more refined type
                  ($write (make-ref-type #t '$proc))
                  ($seek (make-ref-type #t '$proc)) ;; (port, offset, whence) -> offset
                  ($close (make-ref-type #t '$proc)) ;; (port) -> ()
                  ($get-natural-buffer-sizes (make-ref-type #t '$proc)) ;; port -> (rdsz, wrsz)
                  ($random-access? (make-ref-type #t '$proc)) ;; port -> bool
                  ($input-waiting (make-ref-type #t '$proc)) ;; port -> bool
                  ($truncate (make-ref-type #t '$proc)) ;; (port, length) -> ()
                  ;; Guile also has GOOPS classes here.
                  )
         (struct ($port $heap-object)
                 ($pt (make-ref-type #f '$port-type))
                 ($stream (mut scm-type))
                 ($file_name (mut scm-type))
                 ($position (make-ref-type #f '$mutable-pair))
                 ($read_buf (mut scm-type))      ;; A 5-vector
                 ($write_buf (mut scm-type))     ;; A 5-vector
                 ($write_buf_aux (mut scm-type)) ;; A 5-vector
                 ($read_buffering (mut 'i32))
                 ($refcount (mut 'i32))
                 ($rw_random (mut 'i8))
                 ($properties (mut scm-type)))
         (struct ($struct $heap-object)
                 ;; Vtable link is mutable so that we can tie the knot for
                 ;; top types.
                 ($vtable (mut (make-ref-type #t '$vtable))))
         (struct ($vtable $struct)
                 ($vtable (mut (make-ref-type #t '$vtable)))
                 ($field0 (mut scm-type))
                 ($field1 (mut scm-type))
                 ($field2 (mut scm-type))
                 ($field3 (mut scm-type))))))))

  (define types (append func-types heap-types))

  (define-syntax imported-function-type
    (syntax-rules (=>)
      ((_ (param-type ...) => (result-type ...))
       (make-type-use #f (make-func-sig (list (make-param #f param-type)
                                              ...)
                                        (list result-type ...))))))
  (define imports
    (list
     (make-import "rt" "get_argument" 'func '$get-argument
                  (imported-function-type ('i32) => (scm-type)))
     (make-import "rt" "prepare_return_values" 'func '$prepare-return-values
                  (imported-function-type ('i32) => ()))
     (make-import "rt" "set_return_value" 'func '$set-return-value
                  (imported-function-type ('i32 scm-type) => ()))
     
     (make-import "rt" "bignum_from_i64" 'func '$bignum-from-i64
                  (imported-function-type ('i64)
                                          => ((make-ref-type #f 'extern))))
     (make-import "rt" "bignum_from_u64" 'func '$bignum-from-u64
                  (imported-function-type ('i64)
                                          => ((make-ref-type #f 'extern))))
     (make-import "rt" "bignum_is_i64" 'func '$bignum-is-i64
                  (imported-function-type ((make-ref-type #f 'extern))
                                          => ('i32)))
     (make-import "rt" "bignum_is_u64" 'func '$bignum-is-u64
                  (imported-function-type ((make-ref-type #f 'extern))
                                          => ('i32)))
     (make-import "rt" "bignum_get_i64" 'func '$bignum-get-i64
                  (imported-function-type ((make-ref-type #f 'extern))
                                          => ('i64)))
     (make-import "rt" "make_weak_map" 'func '$make-weak-map
                  (imported-function-type ()
                                          => ((make-ref-type #f 'extern))))
     (make-import "rt" "weak_map_get" 'func '$weak-map-get
                  (imported-function-type ((make-ref-type #f 'extern))
                                          => (scm-type)))
     (make-import "rt" "weak_map_set" 'func '$weak-map-set
                  (imported-function-type ((make-ref-type #f 'extern)
                                           scm-type scm-type)
                                          => ()))
     (make-import "rt" "weak_map_delete" 'func '$weak-map-delete
                  (imported-function-type ((make-ref-type #f 'extern) scm-type)
                                          => ('i32)))))

  (define funcs
    (list (make-func
           '$pop-return!
           (make-type-use #f (make-func-sig
                              '()
                              (list (make-ref-type #f '$kvarargs))))
           '()
           `((global.get $return-sp)
             (i32.const 1)
             (i32.sub)
             (global.set $return-sp)
             (global.get $return-sp)
             (table.get $return-stack)
             (ref.as_non_null)))
          (make-func
           '$integer-hash
           (make-type-use #f (make-func-sig
                              (list (make-param '$v 'i32))
                              (list 'i32)))
           '()
           '((local.get 0) (i32.const 61) (i32.xor)
             (local.get 0) (i32.const 16) (i32.shr_u)
             (i32.xor) (local.set 0)
             (local.get 0)
             (local.get 0) (i32.const 3) (i32.shl)
             (i32.add) (local.set 0)
             (local.get 0)
             (local.get 0) (i32.const 4) (i32.shr_u)
             (i32.xor) (local.set 0)
             (local.get 0)
             (i32.const #x27d4eb2d)
             (i32.mul) (local.set 0)
             (local.get 0)
             (local.get 0) (i32.const 15) (i32.shr_u)
             (i32.xor)))
          (make-func
           '$finish-heap-object-hash
           (make-type-use #f (make-func-sig
                              (list (make-param '$hash 'i32))
                              '(i32)))
           '()
           `((local.get 0)
             (call $integer-hash)
             (local.tee 0)
             (if #f ,i32-block-type
                 ((local.get 0))
                 ((i32.const 42)
                  (call $integer-hash)))))
          (make-func
           '$string-hash
           (make-type-use #f (make-func-sig
                              (list
                               (make-param '$str (make-ref-type #f 'string)))
                              '(i32)))
           (list (make-local '$iter (make-ref-type #f 'stringview_iter))
                 (make-local '$hash 'i32)
                 (make-local '$codepoint 'i32))
           `((local.get $str)
             (string.as_iter)
             (local.set $iter)
             (block
              $done ,void-block-type
              ((loop $lp ,void-block-type
                     ((local.get $iter)
                      (stringview_iter.next)
                      (local.set $codepoint)
                      (i32.const -1) (local.get $codepoint) (i32.eq)
                      (br_if $done)
                      (local.get $hash) (i32.const 31) (i32.mul)
                      (local.get $codepoint)
                      (i32.add)
                      (local.set $hash)
                      (br $lp)))))
             (local.get $hash)))
          (make-func
           '$string->symbol
           (make-type-use #f (make-func-sig
                              (list
                               (make-param '$str (make-ref-type #f 'string)))
                              (list (make-ref-type #f '$symbol))))
           '()
           ;; FIXME: intern into symtab.  This is getting excruciating
           `((local.get 0) (call $string-hash) (call $finish-heap-object-hash)
             (i32.const 0) (local.get 0) (struct.new $string)
             (struct.new $symbol)))
          (make-func
           '$symbol->keyword
           (make-type-use #f (make-func-sig
                              (list
                               (make-param '$sym (make-ref-type #f '$symbol)))
                              (list (make-ref-type #f '$keyword))))
           '()
           ;; FIXME: intern into kwtab.
           `((local.get 0) (struct.get $symbol 0) (call $finish-heap-object-hash)
             (local.get 0)
             (struct.new $keyword)))))

  ;; Because V8 and binaryen don't really support non-nullable table
  ;; types right now, we currently use nullable tables.  Grr.
  (define tables
    (list (make-table '$argv
                      (make-table-type
                       (make-limits 0 #f)
                       (make-ref-type #t 'eq))
                      #f)
          (make-table '$return-stack
                      (make-table-type
                       (make-limits 0 #f)
                       (make-ref-type #t '$kvarargs))
                      #f)))

  (define memories '())

  (define globals
    (let ((scm-init '((i32.const 0) i31.new)))
      (list (make-global '$arg3 (make-global-type #t scm-type) scm-init)
            (make-global '$arg4 (make-global-type #t scm-type) scm-init)
            (make-global '$arg5 (make-global-type #t scm-type) scm-init)
            (make-global '$arg6 (make-global-type #t scm-type) scm-init)
            (make-global '$arg7 (make-global-type #t scm-type) scm-init)
            (make-global '$return-sp (make-global-type #t 'i32) '((i32.const 0))))))

  (define exports '())
  (define start #f)
  (define elems '())
  (define datas '())
  (define tags '())
  (define strings '())
  (define custom '())

  (make-wasm (append func-types heap-types)
             (if import-abi?
                 (append
                  (map (match-lambda
                        (($ <global> id type init)
                         (make-import "abi" (symbol->string id) 'global id
                                      type)))
                       globals)
                  (map (match-lambda
                        (($ <table> id type init)
                         (make-import "abi" (symbol->string id) 'table id
                                      type)))
                       tables)
                  imports)
                 imports)
             funcs
             (if import-abi? '() tables)
             memories
             (if import-abi? '() globals)
             exports start elems datas tags strings custom))

(define (lower-to-wasm cps)
  ;; interning constants into constant table
  ;; finalizing constant table
  ;; setting init function.
  (define strings '())
  (define heap-constants '())
  (define (func-label k) (string->symbol (format #f "$f~a" k)))
  (define (lower-func kfun body)
    (let ((cps (intmap-select cps body)))
      (define preds (compute-predecessors cps kfun))
      (define idoms (compute-idoms cps kfun))
      (define dom-children (invert-tree idoms))
      (define (merge-cont? label)
        (let lp ((preds (intmap-ref preds label))
                 (has-forward-in-edge? #f))
          (match preds
            (() #f)
            ((pred . preds)
             (if (< pred label)
                 (or has-forward-in-edge?
                     (lp preds #t))
                 (lp preds has-forward-in-edge?))))))
      (define (loop-cont? label)
        (or-map (lambda (pred) (<= label pred))
                (intmap-ref preds label)))
      (define (loop-label label)
        (string->symbol (format #f "$l~a" label)))
      (define (wrap-loop expr label)
        (if (loop-cont? label)
            `(loop ,(loop-label label) ,void-block-type ,expr)
            expr))
      (define (var-label var) (string->symbol (format #f "$v~a" var)))
      (define (local.get var) `(local.get ,(var-label var)))
      (define (local.set var) `(local.set ,(var-label var)))
      (define (local-arg-label idx) (string->symbol (format #f "$arg~a" idx)))
      (define (global-arg-label idx) (string->symbol (format #f "$arg~a" idx)))
      (define (arg-ref idx)
        (cond
         ((< idx 3) `((local.get ,(local-arg-label idx))))
         ((< idx 8) `((global.get ,(global-arg-label idx))))
         (else `((i32.const ,(- idx 8))
                 (table.get $argv)
                 ref.as_non_null))))
      (define (compile-tail exp)
        (define (pass-abi-arguments args)
          (cons
           `(i32.const ,(length args))
           (let lp ((args args) (idx 0))
             (match args
               (()
                (if (< idx 3)
                    (append '((i32.const 0)
                              (i31.new))
                            (lp args (1+ idx)))
                    '()))
               ((arg . args)
                (cons (cond
                       ((< idx 3) (local.get arg))
                       ((< idx 8)
                        `(global.set ,(global-arg-label idx)
                                     ,(local.get arg)))
                       (else
                        `(table.set $argv (i32.const ,(- idx 8))
                                    ,(local.get arg))))
                      (lp args (1+ idx))))))))
        (match exp
          (($ $call proc args)
           `(,@(pass-abi-arguments args)
             ,(local.get proc)
             (ref.cast $proc)
             (struct.get $proc 1)
             (return_call_ref ,(make-type-use '$kvarargs kvarargs-sig))))
          (($ $calli args callee)
           ;; This is a return.
           `(,@(pass-abi-arguments args)
             ,(local.get callee)
             (return_call_ref ,(make-type-use '$kvarargs kvarargs-sig))))
          (($ $callk k proc args)
           `(,@(map local.get (if proc (cons proc args) args))
             (return_call ,(func-label k))))))
      (define (compile-values exp)
        (define (fixnum? val)
          (and (exact-integer? val)
               (<= (ash -1 -29) val (1- (ash 1 29)))))
        (match exp
          (($ $const val)
           (match val
             ((? fixnum?) `((i32.const ,(ash val 1))
                            (i31.new)))
             (_ (error "unimplemented constant" val))))
          (($ $primcall 'restore1 'ptr ())
           `((call $pop-return!)))
          (_
           (error "unimplemented!" exp))))
      (define (compile-receive exp req rest kargs)
        (match exp
          (_
           (error "unimplemented!!" exp req rest kargs))))
      (define (compile-test op param args)
        (match op
          (_ (error "unimplemented!!!" op param args))))
         
      ;; See "Beyond Relooper: Recursive Translation of Unstructured
      ;; Control Flow to Structured Control Flow", Norman Ramsey, ICFP
      ;; 2022.
      (define (make-ctx next-label stack) (cons next-label stack))
      (define (push-loop label ctx)
        (match ctx
          ((next-label . stack)
           (make-ctx label
                     (acons 'loop-headed-by label stack)))))
      (define (push-block label ctx)
        (match ctx
          ((next-label . stack)
           (make-ctx label
                     (acons 'block-followed-by label stack)))))
      (define (push-if label ctx)
        (match ctx
          ((next-label . stack)
           (make-ctx next-label (cons 'if-then-else stack)))))
      (define (lookup-label k ctx)
        (match ctx
          ((next-label . stack)
           (let lp ((stack stack) (depth 0))
             (match stack
               (('if-then-else . stack) (lp stack (1+ depth)))
               ((((or 'loop-headed-by 'block-followed-by) label) . stack)
                (if (eqv? label k)
                    depth
                    (lp stack (1+ depth))))
               (_ (error "block label not found" k)))))))

      (define (do-tree label ctx)
        (define (code-for-label ctx)
          ;; here if label is a switch we node-within all children
          ;; instead of only merge nodes.
          (define children
            (intset-filter merge-cont? (intmap-ref dom-children label)))
          (node-within label children ctx))
        (if (loop-cont? label)
            `((loop #f #f ,(code-for-label (push-loop label ctx))))
            (code-for-label ctx)))
      (define (do-branch pred succ ctx)
        (cond
         ((or (<= succ pred)
              (merge-cont? succ))
          ;; Backward branch or branch to merge: jump.
          (match ctx
            ((next-label . stack)
             (if (eqv? succ next-label)
                 '()
                 `((br ,(lookup-label succ ctx)))))))
         (else
          ;; Otherwise render successor inline.
          (do-tree succ ctx))))
      (define (node-within label ys ctx)
        (call-with-values (lambda () (intset-pop ys))
          (lambda (ys y)
            (match y
              (#f
               (match (intmap-ref cps label)
                 (($ $kargs names vars term)
                  ;; could change to bind names at continuation?
                  (match term
                    (($ $continue k src exp)
                     (match (intmap-ref cps k)
                       (($ $ktail)
                        (compile-tail exp))
                       (($ $kargs _ vars)
                        `(,@(compile-values exp)
                          ,@(reverse (map local.set vars))
                          ,@(do-branch label k ctx)))
                       (($ $kreceive ($ $arity req () rest () #f) kargs)
                        (compile-receive exp req rest kargs))))
                    (($ $branch kf kt src op param args)
                     `(,@(compile-test op param args)
                       (if #f ,void-block-type
                           ,(do-branch label kt (cons 'if-then-else ctx))
                           ,(do-branch label kf (cons 'if-then-else ctx)))))
                    (($ $switch kf kt* src arg)
                     (error "switch unimplemented"))
                    (($ $prompt k kh src escape? tag)
                     (error "prompts should be removed by tailification?"))
                    (($ $throw src op param args)
                     (error "throw unimplemented"))))
                 (($ $kreceive ($ $arity req () rest () #f) kbody)
                  (error "kreceive unimplemented"))
                 (($ $kfun src meta self ktail kentry)
                  (if self
                      ;; only if referenced?
                      `(,@(arg-ref 0)
                        ,(local.set self)
                        ,@(do-branch label kentry ctx))
                      (do-tree kentry ctx)))
                 (($ $kclause ($ $arity req opt rest kw allow-other-keys?)
                     kbody kalt)
                  (when kalt (error "case-lambda unimplemented"))
                  (when allow-other-keys? (error "allow-other-keys? unimplemented"))
                  (when (not (null? kw)) (error "kwargs unimplemented"))
                  (when (not (null? opt)) (error "optargs unimplemented"))
                  (when rest (error "rest args unimplemented"))
                  (match (intmap-ref cps kbody)
                    (($ $kargs names vars)
                     `((local.get $nargs)
                       (i32.const ,(1+ (length req)))
                       (i32.eq)
                       (if #f ,void-block-type
                           (,@(append-map (lambda (arg idx)
                                            `(,(arg-ref (1+ idx))
                                              ,(local.set arg)))
                                          vars (iota (length req)))
                            ,@(do-branch label kbody ctx))
                           ((unreachable)))))))
                 (($ $ktail)
                  '())))
              (y
               `((block #f ,void-block-type
                        ,(node-within label ys (push-block label ctx)))
                 ,@(do-tree y ctx)))))))
      (define code (do-tree kfun (make-ctx #f '())))
      (define (type-for-repr repr)
        (match repr
          ('scm scm-type)
          ('f64 'f64)
          ((or 's64 'u64) 'i64)
          ('ptr (make-ref-type #f '$kvarargs))))
      (define locals
        (intmap-fold-right (lambda (var repr out)
                             (cons (make-local (var-label var)
                                               (type-for-repr repr))
                                   out))
                           (compute-var-representations cps) '()))
      ;; FIXME: Here attach a name, other debug info to the function
      (make-func (func-label kfun)
                 (make-type-use '$kvarargs kvarargs-sig)
                 locals
                 code)))

  (define (compute-globals)
    ;; FIXME: heap constants
    (list (make-global '$load
                       (make-global-type #t scm-type)
                       `((i32.const 0) (i31.new)))))

  (define (compute-exports)
    (list (make-export "$load" 'global '$load)))

  (define (compute-start-function funcs)
    (define load-function-id
      (match funcs
        ;; Assume that the first function is the "load" function.
        ((($ <func> id) . _) id)))
    (make-func '$start
               void-block-type
               '()
               `((i32.const 0)
                 (ref.func ,load-function-id)
                 (struct.new $proc)
                 (global.set $load))))

  (let* ((funcs
          (intmap-map->list lower-func (compute-reachable-functions cps 0)))
         (start-func (compute-start-function funcs))
         (types '())
         (imports '())
         (tables '())
         (memories '())
         (globals (compute-globals))
         (exports (compute-exports))
         (start '$start)
         (elems '())
         (datas '())
         (tags '())
         (custom '()))
    (make-wasm types imports
               (cons start-func funcs)
               tables memories globals exports
               start elems datas tags strings custom)))

(define* (compile-to-wasm input-file output-file #:key
                          (import-abi? #f)
                          (export-abi? #t)
                          (from (current-language))
                          (env (default-environment from))
                          (optimization-level (default-optimization-level))
                          (warning-level (default-warning-level))
                          (dump-cps? #f)
                          (dump-wasm? #f)
                          (opts '())
                          (canonicalization 'relative))
  (define (compile-to-cps in)
    ;; FIXME: Right now the tree-il->cps phase will expand
    ;; primitives to Guile VM primitives, e.g. including
    ;; `heap-object?` and so on.  We need to instead expand into
    ;; more wasm-appropriate primitives, at some point anyway.
    (define cps
      (read-and-compile in #:env env #:from from #:to 'cps
                        #:optimization-level optimization-level
                        #:warning-level warning-level))
    (define lower-cps
      (let ((make-lower (language-lowerer (lookup-language 'cps))))
        (make-lower optimization-level opts)))
    (define lowered-cps (lower-cps cps env))
    (define tailified (tailify lowered-cps))
    (verify tailified)
    (renumber (simplify (eliminate-dead-code tailified))))
  (call-with-input-file input-file
    (lambda (in)
      (set-port-encoding! in (or (file-encoding in) "UTF-8"))
      (define cps (compile-to-cps in))
      (when dump-cps?
        (dump cps))
      (let* ((wasm (lower-to-wasm cps))
             (wasm (if export-abi? (export-abi wasm) wasm))
             (wasm (add-stdlib wasm (compute-stdlib import-abi?)))
             (wasm (resolve-wasm wasm)))
        (when dump-wasm?
          (format #t "\n\nThe wasm we are going to emit:\n")
          (dump-wasm wasm))
        (let ((bytes (assemble-wasm wasm)))
          (call-with-output-file output-file
            (lambda (out)
              (put-bytevector out bytes))))))))
