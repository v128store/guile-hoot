;;; WebAssembly linker
;;; Copyright (C) 2023 Igalia, S.L.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Linker for WebAssembly, to augment a wasm module by pulling in
;;; missing definitions from a standard library.
;;;
;;; Code:

(define-module (hoot compile)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map))
  #:use-module (srfi srfi-9)
  #:use-module ((system base compile)
                #:select ((read-and-compile . %read-and-compile)
                          (compile . %compile)
                          default-warning-level
                          default-optimization-level))
  #:use-module (system base language)
  #:use-module (system base target)
  #:use-module (language cps)
  #:use-module (language cps intset)
  #:use-module (language cps intmap)
  #:use-module (language cps dump)
  #:use-module (language cps utils)
  #:use-module (rnrs bytevectors)
  #:use-module (hoot stdlib)
  #:use-module (wasm assemble)
  #:use-module (wasm dump)
  #:use-module (wasm link)
  #:use-module (wasm resolve)
  #:use-module (wasm types)
  #:export (read-and-compile
            compile-file
            compile))

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
(define scm-block-type (make-type-use #f (make-func-sig '() (list scm-type))))

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
          (make-export "$raw-sp" 'global '$raw-sp)
          (make-export "$scm-sp" 'global '$scm-sp)
          (make-export "$ret-sp" 'global '$ret-sp)
          (make-export "$raw-stack" 'memory '$raw-stack)
          (make-export "$scm-stack" 'table '$scm-stack)
          (make-export "$ret-stack" 'table '$ret-stack)
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

;; Thomas Wang's 32-bit integer hasher, from
;; http://www.cris.com/~Ttwang/tech/inthash.htm.
(define (hash-i32 i)
  ;; 32-bit hash
  (define (i32 i) (logand i #xffffffff))
  (let* ((i (i32 i))
         (i (i32 (logxor (logxor i 61) (ash i -16))))
         (i (i32 (+ i (i32 (ash i 3)))))
         (i (i32 (logxor i (ash i -4))))
         (i (i32 (* i #x27d4eb2d))))
    (i32 (logxor i (ash i -15)))))
(define (finish-heap-object-hash h)
  (let ((h (hash-i32 h)))
    (if (= h 0)
        (hash-i32 42)
        h)))
(define (hashq-constant x)
  (finish-heap-object-hash (hash x (ash 1 32))))
(define (hashq-symbol x)
  (finish-heap-object-hash
   (string-fold (lambda (ch h)
                  (logand #xffffffff (+ (* h 31) (char->integer ch))))
                0
                (symbol->string x))))
(define (hashq-keyword x)
  (finish-heap-object-hash (hashq-symbol (keyword->symbol x))))

(define-record-type <static-procedure>
  (make-static-procedure code)
  static-procedure?
  (code static-procedure-code))

(define (compute-used-vars cps)
  (define (adjoin var used)
    (intset-add! used var))
  (define (adjoin* vars used)
    (fold1 adjoin vars used))
  (persistent-intset
   (intmap-fold
    (lambda (k cont used)
      ;; Only a term can use a var.
      (match cont
        (($ $kargs names syms term)
         (match term
           (($ $continue k src exp)
            (match exp
              (($ $call proc args)
               (adjoin* args (adjoin proc used)))
              (($ $callk k proc args)
               (adjoin* args (if proc (adjoin proc used) used)))
              (($ $calli args callee)
               (adjoin* args (adjoin callee used)))
              (($ $primcall name param args)
               (adjoin* args used))
              (($ $values args)
               (adjoin* args used))
              ((or ($ $const) ($ $const-fun) ($ $prim) ($ $code)) used)))
           (($ $branch kf kt src op param args)
            (adjoin* args used))
           (($ $switch kf kt* src arg)
            (adjoin arg used))
           (($ $prompt k kh src escape? tag)
            (adjoin tag used))
           (($ $throw src op param args)
            (adjoin* args used))))
        (_ used)))
    cps empty-intset)))

(define* (lower-to-wasm cps #:key import-abi?)
  ;; interning constants into constant table
  ;; finalizing constant table
  ;; setting init function.
  (define strings '())
  (define heap-constants '())
  (define heap-constant-count 0)
  (define heap-constant-names (make-hash-table))
  (define (intern-heap-constant! x)
    (define* (intern! type init-expr #:optional make-reloc)
      (let ((name (string->symbol
                   (format #f "$constant~a" heap-constant-count))))
        (set! heap-constant-count (1+ heap-constant-count))
        (hash-set! heap-constant-names x name)
        (define entry (vector name type init-expr make-reloc))
        (set! heap-constants (cons entry heap-constants))
        name))
    (match x
      ((car . cdr)
       (intern! (make-ref-type #f '$pair)
                `((i32.const ,(hashq-constant x))
                  ,@(compile-constant car)
                  ,@(compile-constant cdr)
                  (struct.new $pair))))
      (#(elt ...)
       (intern! (make-ref-type #f '$vector)
                `((i32.const ,(hashq-constant x))
                  ,@(append-map compile-constant elt)
                  (array.new_fixed $raw-scmvector ,(vector-length x))
                  (struct.new $vector))))
      ((? bytevector?)
       ;; FIXME: Probably we should put the initializers in the data
       ;; section instead of using new_fixed.
       (intern! (make-ref-type #f '$bytevector)
                `((i32.const ,(hashq-constant x))
                  ,@(map (lambda (u8) `(i32.const ,u8))
                         (bytevector->u8-list x))
                  (array.new_fixed $raw-bytevector ,(bytevector-length x))
                  (struct.new $bytevector))))
      ((? bitvector?)
       ;; FIXME: Probably we should put the initializers in the data
       ;; section instead of using new_fixed.
       (intern! (make-ref-type #f '$bitvector)
                `((i32.const ,(hashq-constant x))
                  (i32.const ,(bitvector-length x))
                  ,@(let* ((u32v (uniform-array->bytevector x))
                           (u32len (/ (bytevector-length u32v) 4)))
                      (unless (eq? (native-endianness) (endianness little))
                        (error "unsupported"))
                      (let lp ((i 0))
                        (if (< i u32len)
                            (cons `(i32.const
                                    ,(bytevector-s32-native-ref u32v i))
                                  (lp (+ i 4)))
                            '())))
                  (array.new_fixed $raw-bitvector
                                   ,(ash (+ 31 (bitvector-length x)) -5))
                  (struct.new $bitvector))))
      ((? string?)
       (intern! (make-ref-type #f '$string)
                `((i32.const ,(hashq-constant x))
                  (string.const ,x)
                  (struct.new $string))))
      (($ <static-procedure> code)
       (intern! (make-ref-type #f '$proc)
                `((i32.const 0)
                  (ref.func ,code)
                  (struct.new $proc))))
      ((? symbol?)
       (when import-abi?
         ;; We'd need instead to create this symbol during _start, along
         ;; with any other constant that references it.
         (error "unsupported"))
       (intern! (make-ref-type #f '$symbol)
                `((i32.const ,(hashq-symbol x))
                  ,@(compile-constant (symbol->string x))
                  (struct.new $symbol))
                (lambda (name)
                  `((call $intern-symbol! (global.get ,name))))))
      ((? keyword?)
       (when import-abi?
         ;; We'd need instead to create this keyword during _start, along
         ;; with any other constant that references it.
         (error "unsupported"))
       (intern! (make-ref-type #f '$keyword)
                `((i32.const ,(hashq-keyword x))
                  ,@(compile-constant (keyword->symbol x))
                  (struct.new $keyword))
                (lambda (name)
                  `((call $intern-keyword! (global.get ,name))))))
      (_ (error "unrecognized constant" x))))
  (define (compile-heap-constant val)
    (let ((name (or (hash-ref heap-constant-names val)
                    (intern-heap-constant! val))))
      `((global.get ,name))))
  (define (compile-immediate-constant val)
    (define (fixnum? val)
      (and (exact-integer? val)
           (<= (ash -1 29) val (1- (ash 1 29)))))
    (match val
      ((? fixnum?) `((i32.const ,(ash val 1))
                     (i31.new)))
      (#f `((i32.const 1) (i31.new)))
      ((? (lambda (x) (eq? x #nil))) `((i32.const 5) (i31.new)))
      ((? (lambda (x) (eq? x '()))) `((i32.const 13) (i31.new)))
      (#t `((i32.const 17) (i31.new)))
      ((? unspecified?) `((i32.const 33) (i31.new)))
      ((? eof-object?) `((i32.const 41) (i31.new)))
      ((? char?) `((i32.const ,(logior (ash (char->integer val) 2)
                                       #b11))
                   (i31.new)))
      (_ #f)))
  (define (compile-constant val)
    (or (compile-immediate-constant val)
        (compile-heap-constant val)))
  (define (func-label k) (string->symbol (format #f "$f~a" k)))
  (define (known-arity k)
    (match (intmap-ref cps k)
      (($ $kfun src meta self ktail kentry)
       (match (intmap-ref cps kentry)
         (($ $kclause) #f)
         (($ $kargs names vars) (if self (cons self vars) vars))))))
  (define (lower-func kfun body)
    (let ((cps (intmap-select cps body)))
      (define has-closure?
        (match (intmap-ref cps kfun)
          (($ $kfun src meta self ktail kentry) self)))
      (define elide-arity-check?
        (match (intmap-ref cps kfun)
          (($ $kfun src meta self ktail kentry)
           (assq-ref meta 'elide-arity-check?))))
      (define used-vars (compute-used-vars cps))
      (define (var-used? var) (intset-ref used-vars var))
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
           `(,@(pass-abi-arguments (cons proc args))
             ,(local.get proc)
             (ref.cast #f $proc)
             (struct.get $proc 1)
             (return_call_ref $kvarargs)))
          (($ $calli args callee)
           ;; This is a return.
           `(,@(pass-abi-arguments args)
             ,(local.get callee)
             (return_call_ref $kvarargs)))
          (($ $callk k proc args)
           (let ((args (if proc (cons proc args) args)))
             `(,@(if (known-arity k)
                     (map local.get args)
                     (pass-abi-arguments args))
               (return_call ,(func-label k)))))))

      (define-syntax-rule (match-primcall name param args
                                          ((%name %param . %args) . body) ...)
        (match name
          (%name (match (cons param args) ((%param . %args) . body)))
          ...))

      (define (analyze-saved-vars reprs)
        (define (save/raw memory sp grow-stack idx store-inst alignment)
          (lambda (var sizes)
            (define (prepare-save)
              `((global.get ,sp)
                (local.tee ,sp)
                (i32.const ,(assq-ref sizes sp))
                (i32.add)
                (global.set ,sp)
                (global.get ,sp)
                (i32.const 16) ;; Wasm pages are 64 kB.
                (i32.shr_u)
                (memory.size ,memory)
                (i32.ge_u)
                (if #f ,void-block-type
                    ((i32.const ,(assq-ref sizes sp))
                     (call ,grow-stack))
                    ())))
            `(,@(if (zero? idx) (prepare-save) '())
              (local.get ,sp)
              (local.get ,var)
              (,store-inst ,(make-mem-arg memory idx alignment)))))
        (define (save/ref table sp grow-stack idx)
          (lambda (var sizes)
            (define (prepare-save)
              `((global.get ,sp)
                (local.tee ,sp)
                (i32.const ,(assq-ref sizes sp))
                (i32.add)
                (global.set ,sp)
                (global.get ,sp)
                (table.size ,table)
                (i32.ge_u)
                (if #f ,void-block-type
                    ((i32.const ,(assq-ref sizes sp))
                     (call ,grow-stack))
                    ())))
            `(,@(if (zero? idx) (prepare-save) '())
              (local.get ,sp)
              ,@(if (zero? idx) '() `((i32.const ,idx) (i32.add)))
              (local.get ,var)
              (table.set ,table))))
        (define (restore sp idx code)
          (lambda (sizes)
            (define (prepare-restore)
              `((global.get ,sp)
                (i32.const ,(assq-ref sizes sp))
                (i32.sub)
                (local.tee ,sp)
                (global.set ,sp)))
            `(,@(if (zero? idx) (prepare-restore) '())
              (local.get ,sp)
              ,@code)))
        (define (restore/raw memory sp idx load-inst alignment)
          (restore sp idx `((,load-inst ,(make-mem-arg memory idx alignment)))))
        (define (restore/ref table sp idx)
          (restore sp idx `(,@(if (zero? idx) '() `((i32.const ,idx) (i32.add)))
                            (table.get ,table)
                            (ref.as_non_null))))

        (define (visit/raw idx store-inst load-inst alignment)
          (cons (save/raw '$raw-stack '$raw-sp '$grow-raw-stack idx store-inst
                          alignment)
                (restore/raw '$raw-stack '$raw-sp idx load-inst alignment)))
        (define (visit/ref table sp grow idx)
          (cons (save/ref table sp grow idx)
                (restore/ref table sp idx)))

        (define (visit-i64 idx) (visit/raw idx 'i64.store 'i64.load 8))
        (define (visit-f64 idx) (visit/raw idx 'f64.store 'f64.load 8))
        (define (visit-scm idx)
          (visit/ref '$scm-stack '$scm-sp '$grow-scm-stack idx))
        (define (visit-ret idx)
          (visit/ref '$ret-stack '$ret-sp '$grow-ret-stack idx))

        (let lp ((reprs reprs) (out '())
                 (raw-size 0) (scm-size 0) (ret-size 0))
          (match reprs
            (()
             (values (reverse out)
                     `(($raw-sp . ,raw-size)
                       ($scm-sp . ,scm-size)
                       ($ret-sp . ,ret-size))))
            ((r . reprs)
             (match r
               ((or 'u64 's64)
                (lp reprs
                    (cons (visit-i64 raw-size) out)
                    (+ raw-size 8) scm-size ret-size))
               ('f64
                (lp reprs
                    (cons (visit-f64 raw-size) out)
                    (+ raw-size 8) scm-size ret-size))
               ('scm
                (lp reprs
                    (cons (visit-scm scm-size) out)
                    raw-size (1+ scm-size) ret-size))
               ('ptr
                (lp reprs
                    (cons (visit-ret ret-size) out)
                    raw-size scm-size (1+ ret-size))))))))

      (define (compile-unary-op/fixnum-fast-path a block-type
                                                 fast-expr slow-expr)
        `((block #f ,block-type
                 ((block #f ,void-block-type
                         (,(local.get a)
                          (ref.test #f i31)
                          (i32.eqz)
                          (br_if 0)

                          ,(local.get a)
                          (ref.cast #f i31)
                          (i31.get_s)
                          (local.tee $i0)
                          (i32.const 1)
                          (i32.and)
                          (br_if 0)

                          ,@fast-expr
                          (br 1)))
                  ,(local.get a)
                  ,@slow-expr))))

      (define (compile-binary-op/fixnum-fast-path a b block-type
                                                  fast-expr slow-expr)
        `((block #f ,block-type
                 ((block #f ,void-block-type
                         (,(local.get a)
                          (ref.test #f i31)
                          (i32.eqz)
                          (br_if 0)
                          ,(local.get b)
                          (ref.test #f i31)
                          (i32.eqz)
                          (br_if 0)

                          ,(local.get a)
                          (ref.cast #f i31)
                          (i31.get_s)
                          (local.tee $i0)
                          ,(local.get b)
                          (ref.cast #f i31)
                          (i31.get_s)
                          (local.tee $i1)
                          (i32.or)
                          (i32.const 1)
                          (i32.and)
                          (br_if 0)

                          ,@fast-expr
                          (br 1)))
                  ,(local.get a)
                  ,(local.get b)
                  ,@slow-expr))))

      (define (compile-values exp)
        (match exp
          (($ $const val) (compile-constant val))
          (($ $const-fun k) (compile-constant
                             (make-static-procedure (func-label k))))
          (($ $values vals)
           (map local.get vals))

          (($ $code k)
           `((ref.func ,(func-label k))))

          (($ $primcall name param args)
           (match-primcall
            name param args

            ;; These are the primcalls inserted by tailification to
            ;; handle stack-allocated return continuations.
            (('save reprs . args)
             (call-with-values (lambda () (analyze-saved-vars reprs))
               (lambda (analyzed spill-sizes)
                 (define (save-var analyzed var)
                   (match analyzed
                     ((save . restore)
                      (save (var-label var) spill-sizes))))
                 (append-map save-var analyzed args))))
            (('drop reprs)
             (call-with-values (lambda () (analyze-saved-vars reprs))
               (lambda (analyzed spill-sizes)
                 (define pop-sp
                   (match-lambda
                    ((sp . size)
                     (if (zero? size)
                         '()
                         `((global.get ,sp)
                           (i32.const ,size)
                           (i32.sub)
                           (global.set ,sp))))))
                 (append-map pop-sp spill-sizes))))
            (('restore reprs)
             ;; Precondition: the order of the vars (and associated
             ;; reprs) is the same as for the corresponding save.
             (call-with-values (lambda () (analyze-saved-vars reprs))
               (lambda (analyzed spill-sizes)
                 (define restore-var
                   (match-lambda
                    ((save . restore)
                     (restore spill-sizes))))
                 (append-map restore-var analyzed))))
            (('push-prompt escape? tag handler)
             (error "unimplemented" exp))

            ;; WebAssembly doesn't need loop instrumentation.
            (('instrument-loop #f)
             '())

            ;; Primcalls related to the module system.  Not sure if we
            ;; will need these, at least in this form; the whole-program
            ;; compiler may take a different approach to modules.
            (('current-module #f)
             (error "unimplemented" exp))
            (('current-thread #f)
             (error "unimplemented" exp))
            (('define! #f mod sym)
             (error "unimplemented" exp))
            (('resolve (bound?) (name))
             (error "unimplemented" exp))
            (('cache-ref key)
             (error "unimplemented" exp))
            (('cache-set! key val)
             (error "unimplemented" exp))
            (('resolve-module public? (name))
             (error "unimplemented" exp))
            (('module-variable #f mod name)
             (error "unimplemented" exp))
            (('lookup #f mod name)
             (error "unimplemented" exp))
            (('lookup-bound #f mod name)
             (error "unimplemented" exp))
            (('lookup-bound-public (mod name))
             (error "unimplemented" exp))
            (('lookup-bound-private (mod name))
             (error "unimplemented" exp))

            ;; Object allocation.  Probably most of these need to be
            ;; replaced with `cons` et al; see log.md.
            (('cons #f head tail)
             `((i32.const 0)
               ,(local.get head)
               ,(local.get tail)
               (struct.new $pair)))
            (('car #f pair)
             `(,(local.get pair)
               (ref.cast #f $pair)
               (struct.get $pair $car)))
            (('cdr #f pair)
             `(,(local.get pair)
               (ref.cast #f $pair)
               (struct.get $pair $cdr)))
            (('set-car! #f pair val)
             `(,(local.get pair)
               (ref.cast #f $pair)
               ,(local.get val)
               (struct.set $pair $car)))
            (('set-cdr! #f pair val)
             `(,(local.get pair)
               (ref.cast #f $pair)
               ,(local.get val)
               (struct.set $pair $cdr)))

            (('allocate-vector/immediate len)
             `((i32.const 0)
               ,@(compile-constant #f)
               (i32.const ,len)
               (array.new $raw-scmvector)
               (struct.new $vector)))
            (('allocate-vector #f len)
             `((i32.const 0)
               ,@(compile-constant #f)
               ,(local.get len)
               (i32.wrap_i64)
               (array.new $raw-scmvector)
               (struct.new $vector)))
            (('vector-length #f v)
             `(,(local.get v)
               (ref.cast #f $vector)
               (struct.get $vector $vals)
               (array.len)
               (i64.extend_i32_u)))
            (('vector-ref #f v idx)
             `(,(local.get v)
               (ref.cast #f $vector)
               (struct.get $vector $vals)
               ,(local.get idx)
               (i32.wrap_i64)
               (array.get $raw-scmvector)))
            (('vector-ref/immediate idx v)
             `(,(local.get v)
               (ref.cast #f $vector)
               (struct.get $vector $vals)
               (i32.const ,idx)
               (array.get $raw-scmvector)))
            (('vector-set! #f v idx val)
             `(,(local.get v)
               (ref.cast #f $vector)
               (struct.get $vector $vals)
               ,(local.get idx)
               (i32.wrap_i64)
               ,(local.get val)
               (array.set $raw-scmvector)))
            (('vector-set!/immediate idx v val)
             `(,(local.get v)
               (ref.cast #f $vector)
               (struct.get $vector $vals)
               (i32.const ,idx)
               ,(local.get val)
               (array.set $raw-scmvector)))

            (('box #f val)
             `((i32.const 0)
               ,(local.get val)
               (struct.new $variable)))
            (('box-ref #f v)
             `(,(local.get v)
               (ref.cast #f $variable)
               (struct.get $variable $val)))
            (('box-set! #f v val)
             `(,(local.get v)
               (ref.cast #f $variable)
               ,(local.get val)
               (struct.set $variable $val)))

            (('bv-length #f bv)
             `(,(local.get bv)
               (ref.cast #f $bytevector)
               (struct.get $bytevector $vals)
               (array.len)
               (i64.extend_i32_u)))
            (('bv-contents #f bv)
             ;; Need to model bv-contents in a way other than 'ptr.
             (error "unimplemented" exp))

            (('make-closure nfree code)
             `((i32.const 0)
               ,(local.get code)
               ,@(make-list nfree '(i32.const 0))
               (struct.new ,(closure-type nfree))))
            (('closure-ref (idx . nfree) closure)
             (let ((type (closure-type nfree))
                   ;; Two header fields: hash and proc.
                   (pos (+ idx 2)))
               `(,(local.get closure)
                 (ref.cast #f ,type)
                 (struct.get ,type ,pos))))
            (('closure-set! (idx . nfree) closure val)
             (let ((type (closure-type nfree))
                   ;; Two header fields: hash and proc.
                   (pos (+ idx 2)))
               `(,(local.get closure)
                 (ref.cast #f ,type)
                 ,(local.get val)
                 (struct.set ,type ,pos))))

            (('struct-vtable #f struct)
             (error "unimplemented" exp))
            (('vtable-size #f struct)
             (error "unimplemented" exp))
            (('allocate-struct nfields vtable)
             (error "unimplemented" exp))
            (('struct-ref idx struct)
             (error "unimplemented" exp))
            (('struct-set! idx struct val)
             (error "unimplemented" exp))

            (('string-length #f str)
             (error "unimplemented" exp))
            (('string-ref #f str idx)
             (error "unimplemented" exp))
            (('string-set! #f str idx val)
             (error "unimplemented" exp))

            (('make-atomic-box #f val)
             (error "unimplemented" exp))
            (('atomic-box-ref #f box)
             (error "unimplemented" exp))
            (('atomic-box-set! #f box val)
             (error "unimplemented" exp))
            (('atomic-box-swap! #f box val)
             (error "unimplemented" exp))
            (('atomic-box-compare-and-swap! #f box expected desired)
             (error "unimplemented" exp))

            (('f64->scm #f f64)
             (error "unimplemented" exp))

            ;; Generic arithmetic.  Emit a fixnum fast-path and a
            ;; callout to runtime functions for the slow path.
            (('add #f x y)
             (compile-binary-op/fixnum-fast-path
              x y scm-block-type
              ;; FIXME: Overflow to bignum.
              '((local.get $i0) (local.get $i1) (i32.add) (i31.new))
              '((unreachable))))
            (('sub #f x y)
             (compile-binary-op/fixnum-fast-path
              x y scm-block-type
              ;; FIXME: Overflow to bignum.
              '((local.get $i0) (local.get $i1) (i32.sub) (i31.new))
              '((unreachable))))
            (('add/immediate y x)
             (compile-unary-op/fixnum-fast-path
              x scm-block-type
              ;; FIXME: Overflow to bignum.
              `((local.get $i0) (i32.const ,(ash y 1)) (i32.add) (i31.new))
              '((unreachable))))
            (('sub/immediate y x)
             (compile-unary-op/fixnum-fast-path
              x scm-block-type
              ;; FIXME: Overflow to bignum.
              `((local.get $i0) (i32.const ,(ash y 1)) (i32.sub) (i31.new))
              '((unreachable))))
            (('mul #f x y)
             (compile-binary-op/fixnum-fast-path
              x y scm-block-type
              ;; FIXME: Overflow to bignum.
              '((local.get $i0)
                (local.get $i1) (i32.const 1) (i32.shr_s)
                (i32.mul) (i31.new))
              '((unreachable))))
            (('div #f x y)
             (error "unimplemented" exp))
            (('quo #f x y)
             (error "unimplemented" exp))
            (('rem #f x y)
             (error "unimplemented" exp))
            (('mod #f x y)
             (error "unimplemented" exp))

            ;; Integer bitwise operations.  Fast path for fixnums and
            ;; callout otherwise.
            (('logand #f x y)           ;FIXME: implement slow path
             (compile-binary-op/fixnum-fast-path
              x y scm-block-type
              '((local.get $i0) (local.get $i1) (i32.and) (i31.new))
              '((unreachable))))
            (('logior #f x y)           ;FIXME: implement slow path
             (compile-binary-op/fixnum-fast-path
              x y scm-block-type
              '((local.get $i0) (local.get $i1) (i32.or) (i31.new))
              '((unreachable))))
            (('logxor #f x y)           ;FIXME: implement slow path
             (compile-binary-op/fixnum-fast-path
              x y scm-block-type
              '((local.get $i0) (local.get $i1) (i32.xor) (i31.new))
              '((unreachable))))
            (('logsub #f x y)           ;FIXME: implement slow path
             (compile-binary-op/fixnum-fast-path
              x y scm-block-type
              '((local.get $i0)
                (local.get $i1)
                (i32.const -1)
                (i32.xor)
                (i32.and)
                (i31.new))
              '((unreachable))))
            (('rsh #f x y)
             (error "unimplemented" exp))
            (('lsh #f x y)
             (error "unimplemented" exp))
            (('rsh/immediate y x)
             (error "unimplemented" exp))
            (('lsh/immediate y x)
             (error "unimplemented" exp))

            ;; Arithmetic on real numbers.
            (('inexact #f x)
             (error "unimplemented" exp))
            (('abs #f x)
             (error "unimplemented" exp))
            (('sqrt #f x)
             (error "unimplemented" exp))
            (('floor #f x)
             (error "unimplemented" exp))
            (('ceiling #f x)
             (error "unimplemented" exp))

            ;; Trig functions.  Probably just call out for now.
            (('sin #f x)
             (error "unimplemented" exp))
            (('cos #f x)
             (error "unimplemented" exp))
            (('tan #f x)
             (error "unimplemented" exp))
            (('asin #f x)
             (error "unimplemented" exp))
            (('acos #f x)
             (error "unimplemented" exp))
            (('atan #f x)
             (error "unimplemented" exp))
            (('atan2 #f x y)
             (error "unimplemented" exp))

            ;; Unboxed integer arithmetic and logical operations.
            (((or 'load-s64 'load-u64) s64)
             `((i64.const ,s64)))
            (('load-s64 s64)
             `((i64.const ,s64)))
            (((or 's64->u64 'u64->s64) #f arg)
             `(,(local.get arg)))
            (('uadd #f x y)
             `(,(local.get x)
               ,(local.get y)
               (i64.add)))
            (('usub #f x y)
             `(,(local.get x)
               ,(local.get y)
               (i64.sub)))
            (('umul #f x y)
             `(,(local.get x)
               ,(local.get y)
               (i64.mul)))
            (('uadd/immediate y x)
             `(,(local.get x)
               (i64.const ,y)
               (i64.add)))
            (('usub/immediate y x)
             `(,(local.get x)
               (i64.const ,y)
               (i64.sub)))
            (('umul/immediate y x)
             `(,(local.get x)
               (i64.const ,y)
               (i64.mul)))
            (('ursh #f x y)
             `(,(local.get x)
               ,(local.get y)
               (i64.shr_u)))
            (('srsh #f x y)
             `(,(local.get x)
               ,(local.get y)
               (i64.shr_s)))
            (('ulsh #f x y)
             `(,(local.get x)
               ,(local.get y)
               (i64.shl)))
            (('ursh/immediate y x)
             `(,(local.get x)
               (i64.const ,y)
               (i64.shr_u)))
            (('srsh/immediate y x)
             `(,(local.get x)
               (i64.const ,y)
               (i64.shr_s)))
            (('ulsh/immediate y x)
             `(,(local.get x)
               (i64.const ,y)
               (i64.shl)))

            ;; Unboxed floating-point arithmetic and trig.
            (('load-f64 f64)
             `((f64.const ,f64)))
            (('s64->f64 #f arg)
             (error "unimplemented" exp))
            (('fadd #f x y)
             (error "unimplemented" exp))
            (('fsub #f x y)
             (error "unimplemented" exp))
            (('fmul #f x y)
             (error "unimplemented" exp))
            (('fdiv #f x y)
             (error "unimplemented" exp))
            (('fabs #f x)
             (error "unimplemented" exp))
            (('fsqrt #f x)
             (error "unimplemented" exp))
            (('ffloor #f x)
             (error "unimplemented" exp))
            (('fceiling #f x)
             (error "unimplemented" exp))
            (('fsin #f x)
             (error "unimplemented" exp))
            (('fcos #f x)
             (error "unimplemented" exp))
            (('ftan #f x)
             (error "unimplemented" exp))
            (('fasin #f x)
             (error "unimplemented" exp))
            (('facos #f x)
             (error "unimplemented" exp))
            (('fatan #f x)
             (error "unimplemented" exp))
            (('fatan2 #f x y)
             (error "unimplemented" exp))

            ;; Misc.
            (('string->bignum #f x)
             (error "unimplemented" exp))
            (('string->symbol #f x)
             (error "unimplemented" exp))
            (('symbol->keyword #f x)
             (error "unimplemented" exp))

            ;; Unboxing and boxing numbers.
            (('scm->f64 #f src)
             (error "unimplemented" exp))
            (('scm->u64 #f src)
             (error "unimplemented" exp))
            (('scm->u64/truncate #f src)
             (error "unimplemented" exp))
            (('u64->scm #f src)
             `(,(local.get src)
               (i64.const ,(ash 1 29))
               (i64.lt_u)
               (if #f ,scm-block-type
                   (,(local.get src)
                    (i32.wrap_i64)
                    (i32.const 1)
                    (i32.shl)
                    (i31.new))
                   (,(local.get src)
                    (call $u64->bignum)))))
            (('scm->s64 #f src)
             (error "unimplemented" exp))
            (('s64->scm #f src)
             (error "unimplemented" exp))

            ;; For native Guile, these bytevector accesses take three
            ;; parameters: the object itself, which is unused but keeps
            ;; the pointer alive; a pointer to the actual bytes; and an
            ;; index.  For WebAssembly we'll pass the $raw-bytevector as
            ;; the ptr and not reference the object.  Annoyingly there
            ;; are no native multi-byte accesses to i8 arrays.
            (('u8-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('s8-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('u16-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('s16-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('u32-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('s32-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('u64-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('s64-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('f32-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('f64-ref ann obj ptr idx)
             (error "unimplemented" exp))
            (('u8-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('s8-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('u16-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('s16-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('u32-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('s32-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('u64-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('s64-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('f32-set! ann obj ptr idx val)
             (error "unimplemented" exp))
            (('f64-set! ann obj ptr idx val)
             (error "unimplemented" exp))

            ;; Infallible unboxing of fixnums and chars.
            (('untag-fixnum #f src)
             `(,(local.get src)
               (ref.cast #f i31)
               (i31.get_s)
               (i32.const 1)
               (i32.shr_s)
               (i64.extend_i32_s)))
            (('tag-fixnum #f src)
             `(,(local.get src)
               (i32.wrap_i64)
               (i32.const 1)
               (i32.shl)
               (i31.new)))
            (('untag-char #f src)
             `(,(local.get src)
               (ref.cast #f i31)
               (i31.get_s)
               (i32.const 2)
               (i32.shr_u)
               (i64.extend_i32_u)))
            (('tag-char #f src)
             `(,(local.get src)
               (i32.wrap_i64)
               (i32.const 2)
               (i32.shl)
               (i32.const #b11)
               (i32.or)
               (i31.new)))

            ;; The dynamic state.  Implement using runtime.
            (('push-fluid #f fluid val)
             (error "unimplemented" exp))
            (('pop-fluid #f)
             (error "unimplemented" exp))
            (('fluid-ref #f fluid)
             (error "unimplemented" exp))
            (('fluid-set! #f fluid value)
             (error "unimplemented" exp))
            (('push-dynamic-state #f state)
             (error "unimplemented" exp))
            (('pop-dynamic-state #f)
             (error "unimplemented" exp))
            (('wind #f winder unwinder)
             (error "unimplemented" exp))
            (('unwind #f)
             (error "unimplemented" exp))

            ((name param . args)
             (error "unexpected primcall" name param args))))

          (_ (error "unexpected expression" exp))))

      (define (compile-test op param args)
        (match (vector op param args)
          ;; Immediate type tag predicates.
          (#('fixnum? #f (a))
           `(,(local.get a)
             (ref.test #f i31)
             (if #f ,i32-block-type
                 (,(local.get a)
                  (ref.cast #f i31)
                  (i31.get_s)
                  (i32.const #b1)
                  (i32.and)
                  (i32.eqz))
                 ((i32.const 0)))))
          ;; FIXME: elide heap-object? tests if they just guard a more
          ;; specific test.
          (#('heap-object? #f (a))
           `(,(local.get a) (ref.test #f $heap-object)))
          (#('char? #f (a))
           `(,(local.get a)
             (ref.test #f i31)
             (if #f ,i32-block-type
                 (,(local.get a)
                  (ref.cast #f i31)
                  (i31.get_u)
                  (i32.const #b11)
                  (i32.and)
                  (i32.const #b11)
                  (i32.eq))
                 ((i32.const 0)))))
          (#('eq-constant? imm (a))
           `(,(local.get a)
             ,@(compile-constant imm)
             (ref.eq)))
          (#('undefined? #f (a))
           `(,(local.get a)
             ,@(compile-constant (if #f #f))
             (ref.eq)))
          (#('null? #f (a))
           `(,(local.get a)
             (ref.test #f i31)
             (if #f ,i32-block-type
                 (,(local.get a)
                  (ref.cast #f i31)
                  (i31.get_s)
                  (i32.const #b110111)
                  (i32.and)
                  (i32.const #b001001)
                  (i32.eq))
                 ((i32.const 0)))))
          (#('false? #f (a))
           `(,(local.get a)
             (ref.test #f i31)
             (if #f ,i32-block-type
                 (,(local.get a)
                  (ref.cast #f i31)
                  (i31.get_s)
                  (i32.const #b111011)
                  (i32.and)
                  (i32.const #b000001)
                  (i32.eq))
                 ((i32.const 0)))))
          (#('nil? #f (a))
           `(,(local.get a)
             (ref.test #f i31)
             (if #f ,i32-block-type
                 (,(local.get a)
                  (ref.cast #f i31)
                  (i31.get_s)
                  (i32.const #b110011)
                  (i32.and)
                  (i32.const #b000001)
                  (i32.eq))
                 ((i32.const 0)))))
          (#('pair? #f (a))             `(,(local.get a) (ref.test #f $pair)))
          (#('struct? #f (a))           `(,(local.get a) (ref.test #f $struct)))
          (#('symbol? #f (a))           `(,(local.get a) (ref.test #f $symbol)))
          (#('variable? #f (a))         `(,(local.get a) (ref.test #f $variable)))
          (#('vector? #f (a))           `(,(local.get a) (ref.test #f $vector)))
          (#('mutable-vector? #f (a))   `(,(local.get a) (ref.test #f $mutable-vector)))
          (#('immutable-vector? #f (a)) `(,(local.get a) (ref.test #f $vector)))
          (#('string? #f (a))           `(,(local.get a) (ref.test #f $string)))
          (#('heap-number? #f (a))      `(,(local.get a) (ref.test #f $heap-number)))
          (#('hash-table? #f (a))       `(,(local.get a) (ref.test #f $hash-table)))
          (#('fluid? #f (a))            `(,(local.get a) (ref.test #f $fluid)))
          (#('dynamic-state? #f (a))    `(,(local.get a) (ref.test #f $dynamic-state)))
          (#('keyword? #f (a))          `(,(local.get a) (ref.test #f $keyword)))
          (#('atomic-box? #f (a))       `(,(local.get a) (ref.test #f $atomic-box)))
          (#('syntax? #f (a))           `(,(local.get a) (ref.test #f $syntax)))
          (#('program? #f (a))          `(,(local.get a) (ref.test #f $proc)))
          (#('bytevector? #f (a))       `(,(local.get a) (ref.test #f $bytevector)))
          (#('weak-table? #f (a))       `(,(local.get a) (ref.test #f $weak-table)))
          (#('bitvector? #f (a))        `(,(local.get a) (ref.test #f $bitvector)))
          (#('port? #f (a))             `(,(local.get a) (ref.test #f $port)))
          (#('bignum? #f (a))           `(,(local.get a) (ref.test #f $bignum)))
          (#('flonum? #f (a))           `(,(local.get a) (ref.test #f $flonum)))
          (#('compnum? #f (a))          `(,(local.get a) (ref.test #f $compnum)))
          (#('fracnum? #f (a))          `(,(local.get a) (ref.test #f $fracnum)))
          (#('eq? #f (a b))             `(,(local.get a) ,(local.get b) (ref.eq)))

          (('vtable-vtable? #f (vtable))
           (error "unimplemented branch" op param args))
          ;; FIXME: fold these two in pre-pass.
          (('vtable-has-unboxed-fields? nfields (vtable))
           `((i32.const 0)))
          (('vtable-field-boxed? idx (vtable))
           `((i32.const 1)))
            
          (#('heap-numbers-equal? #f (a b))
           `(,(local.get a) ,(local.get b) (call $heap-numbers-equal?)))
          (#('< #f (a b))
           (compile-binary-op/fixnum-fast-path a b i32-block-type
                                               '((local.get $i0)
                                                 (local.get $i1)
                                                 (i32.lt_s))
                                               '((call $slow-<))))
          (#('<= #f (a b))
           (compile-binary-op/fixnum-fast-path a b i32-block-type
                                               '((local.get $i0)
                                                 (local.get $i1)
                                                 (i32.le_s))
                                               '((call $slow-<=))))
          (#('= #f (a b))
           (compile-binary-op/fixnum-fast-path a b i32-block-type
                                               '((local.get $i0)
                                                 (local.get $i1)
                                                 (i32.eq))
                                               '((call $slow-=))))
          (#('u64-< #f (a b))
           `(,(local.get a)
             ,(local.get b)
             (i64.lt_u)))
          (#('u64-imm-< b (a))
           `(,(local.get a)
             (i64.const ,b)
             (i64.lt_u)))
          (#('imm-u64-< b (a))
           `((i64.const ,b)
             ,(local.get a)
             (i64.lt_u)))
          (#((or 'u64-= 's64-=) #f (a b))
           `(,(local.get a)
             ,(local.get b)
             (i64.eq)))
          (#((or 'u64-imm-= 's64-imm-=) b (a))
           `(,(local.get a)
             (i64.const ,b)
             (i64.eq)))
          (#('s64-< #f (a b))
           `(,(local.get a)
             ,(local.get b)
             (i64.lt_s)))
          (#('s64-imm-< b (a))
           `(,(local.get a)
             (i64.const ,b)
             (i64.lt_s)))
          (#('imm-u64-< b (a))
           `((i64.const ,b)
             ,(local.get a)
             (i64.lt_s)))
          (#('f64-< #f (a b))
           `(,(local.get a)
             ,(local.get b)
             (f64.lt)))
          (#('f64-<= #f (a b))
           `(,(local.get a)
             ,(local.get b)
             (f64.le)))
          (#('f64-= #f (a b))
           `(,(local.get a)
             ,(local.get b)
             (f64.eq)))))
         
      (define (compile-throw src op param args)
        ;; FIXME: Instead of dying, we need to implement exception
        ;; handling, as in Guile.
        (match (vector op param args)
          (#('throw #f (key args))
           `((string.const "throw")
             (i32.const 0)
             ,(local.get key)
             ,(local.get args)
             (struct.new $pair)
             (call $die)
             (unreachable)))
          (#('throw/value param (val))
           `((string.const "throw/value")
             (i32.const 0)
             ,(local.get val)
             ,@(compile-constant param)
             (struct.new $pair)
             (call $die)
             (unreachable)))
          (#('throw/value+data param (val))
           `((string.const "throw/value+data")
             (i32.const 0)
             ,(local.get val)
             ,@(compile-constant param)
             (struct.new $pair)
             (call $die)
             (unreachable)))))

      ;; See "Beyond Relooper: Recursive Translation of Unstructured
      ;; Control Flow to Structured Control Flow", Norman Ramsey, ICFP
      ;; 2022.
      (define (make-ctx next-label stack) (cons next-label stack))
      (define (push-loop label ctx)
        (match ctx
          ((next-label . stack)
           (make-ctx next-label
                     (acons 'loop-headed-by label stack)))))
      (define (push-block label ctx)
        (match ctx
          ((next-label . stack)
           (make-ctx label
                     (acons 'block-followed-by label stack)))))
      (define (push-if ctx)
        (match ctx
          ((next-label . stack)
           (make-ctx next-label (cons 'if-then-else stack)))))
      (define (lookup-label k ctx)
        (match ctx
          ((next-label . stack)
           (let lp ((stack stack) (depth 0))
             (match stack
               (('if-then-else . stack) (lp stack (1+ depth)))
               ((((or 'loop-headed-by 'block-followed-by) . label) . stack)
                (if (eqv? label k)
                    depth
                    (lp stack (1+ depth))))
               (_ (error "block label not found" k ctx)))))))

      (define (do-tree label ctx)
        (define (code-for-label ctx)
          ;; here if label is a switch we node-within all children
          ;; instead of only merge nodes.
          (define children
            (intset-filter merge-cont? (intmap-ref dom-children label)))
          (node-within label children ctx))
        (if (loop-cont? label)
            `((loop #f ,void-block-type
                    ,(code-for-label (push-loop label ctx))))
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
                        (error "kreceive should be tailified away"))))
                    (($ $branch kf kt src op param args)
                     `(,@(compile-test op param args)
                       (if #f ,void-block-type
                           ,(do-branch label kt (push-if ctx))
                           ,(do-branch label kf (push-if ctx)))))
                    (($ $switch kf kt* src arg)
                     (error "switch unimplemented"))
                    (($ $prompt k kh src escape? tag)
                     (error "prompts should be removed by tailification?"))
                    (($ $throw src op param args)
                     (compile-throw src op param args))))
                 (($ $kreceive)
                  (error "kreceive should be tailified away"))
                 (($ $kfun src meta self ktail kentry)
                  (match (intmap-ref cps kentry)
                    (($ $kclause)
                     ;; An arity-checking function; let the clause check
                     ;; the arity.
                     (if (and self (var-used? self))
                         `(,@(arg-ref 0)
                           ,(local.set self)
                           ,@(do-branch label kentry ctx))
                         (do-tree kentry ctx)))
                    (($ $kargs names vars _)
                     ;; A function whose callers all pass the expected
                     ;; number of arguments.
                     (let ((vars (if self (cons self vars) vars)))
                       ;; FIXME: no need to rebind vars.
                       `(,@(append-map (lambda (var idx)
                                         `((local.get ,idx)
                                           ,(local.set var)))
                                       vars (iota (length vars)))
                         ,@(do-tree kentry ctx))))))
                 (($ $kclause ($ $arity req opt rest kw allow-other-keys?)
                     kbody kalt)
                  (when kalt (error "case-lambda unimplemented"))
                  (when allow-other-keys? (error "allow-other-keys? unimplemented"))
                  (when (not (null? kw)) (error "kwargs unimplemented"))
                  (when (not (null? opt)) (error "optargs unimplemented"))
                  (match (intmap-ref cps kbody)
                    (($ $kargs names vars)
                     (when (and rest (var-used? (car (last-pair vars))))
                       (error "rest args unimplemented"))
                     (let ((body `(,@(append-map
                                      (lambda (arg idx)
                                        (if (var-used? arg)
                                            `(,@(arg-ref
                                                 (if has-closure? (1+ idx) idx))
                                              ,(local.set arg))
                                            '()))
                                      vars (iota (length req)))
                                   ,@(do-branch label kbody ctx))))
                       (cond
                        (elide-arity-check? body)
                        (else
                         `((local.get $nargs)
                           (i32.const ,((if has-closure? 1+ identity)
                                        (length req)))
                           ,(if rest '(i32.ge_u) '(i32.eq))
                           (if #f ,void-block-type ,body ((unreachable))))))))))
                 (($ $ktail)
                  '())))
              (y
               `((block #f ,void-block-type
                        ,(node-within label ys (push-block y ctx)))
                 ,@(do-tree y ctx)))))))
      (define code (do-tree kfun (make-ctx #f '())))
      (define (type-for-repr repr)
        (match repr
          ('scm scm-type)
          ('f64 'f64)
          ((or 's64 'u64) 'i64)
          ('ptr (make-ref-type #f '$kvarargs))))
      (define (add-locals-from-code code)
        (define locals (make-hash-table))
        (define (visit-inst inst)
          (match inst
            (((or 'block 'loop) label type body)
             (visit-expr body))
            (('if label type consequent alternate)
             (visit-expr consequent)
             (visit-expr alternate))
            (('try label type body catches catch-all)
             (visit-expr body)
             (for-each visit-expr catches)
             (when catch-all (visit-expr catch-all)))
            (('try_delegate label type body handler)
             (visit-expr body))
            (((or 'local.set 'local.tee) label)
             (let ((type (match label
                           ((or '$raw-sp '$scm-sp '$ret-sp) 'i32)
                           ((or '$i0 '$i1 '$i2) 'i32)
                           (_ #f))))
               (when type
                 (hashq-set! locals label type))))
            (_ #f)))
        (define (visit-expr expr)
          (match expr
            (() (values))
            ((inst . expr)
             (visit-inst inst)
             (visit-expr expr))))
        (visit-expr code)
        (sort (hash-map->list make-local locals)
              (lambda (a b)
                (string<? (match a (($ <local> id) (symbol->string id)))
                          (match b (($ <local> id) (symbol->string id)))))))
      (define locals
        (append
         (add-locals-from-code code)
         (intmap-fold-right (lambda (var repr out)
                              (cons (make-local (var-label var)
                                                (type-for-repr repr))
                                    out))
                            (compute-var-representations cps) '())))
      ;; FIXME: Here attach a name, other debug info to the function
      (make-func (func-label kfun)
                 (match (known-arity kfun)
                   (#f (make-type-use '$kvarargs kvarargs-sig))
                   (vars (make-type-use
                          #f
                          (make-func-sig (map (lambda (_)
                                                (make-param #f scm-type))
                                              vars)
                                         '()))))
                 locals
                 code)))

  (define (compute-globals)
    (fold1 (lambda (entry globals)
             (match entry
               (#(name type init-expr reloc-expr)
                (cons (make-global name
                                   (make-global-type #f type)
                                   init-expr)
                      globals))))
           heap-constants
           (list (make-global '$load
                              (make-global-type #t scm-type)
                              `((i32.const 0) (i31.new))))))

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

(define* (high-level-cps->wasm cps #:key
                               (import-abi? #f)
                               (export-abi? #t)
                               (env #f)
                               (optimization-level #f)
                               (warning-level #f)
                               (dump-cps? #f)
                               (dump-wasm? #f)
                               (opts '()))
  (define (lower-and-tailify cps)
    (with-target "wasm32-unknown-hoot"
      (lambda ()
        (define lower-cps
          (let ((make-lower (language-lowerer (lookup-language 'cps))))
            (make-lower optimization-level opts)))
        (lower-cps cps env))))
  (let ((cps (lower-and-tailify cps)))
    (when dump-cps?
      (dump cps))
    (let* ((wasm (lower-to-wasm cps #:import-abi? import-abi?))
           (wasm (if export-abi? (export-abi wasm) wasm))
           (wasm (add-stdlib wasm (compute-stdlib import-abi?)))
           (wasm (resolve-wasm wasm)))
      (when dump-wasm?
        (format #t "\n\nThe wasm we are going to emit:\n")
        (dump-wasm wasm))
      wasm)))

(define* (compile exp #:key
                  (import-abi? #f)
                  (export-abi? #t)
                  (from (current-language))
                  (env (default-environment from))
                  (optimization-level (default-optimization-level))
                  (warning-level (default-warning-level))
                  (dump-cps? #f)
                  (dump-wasm? #f)
                  (opts '()))
  (define cps
    (%compile exp #:env env #:from from #:to 'cps
              #:optimization-level optimization-level
              #:warning-level warning-level))
  (high-level-cps->wasm cps
                        #:import-abi? import-abi?
                        #:export-abi? export-abi?
                        #:env env
                        #:optimization-level optimization-level
                        #:warning-level warning-level
                        #:dump-cps? dump-cps?
                        #:dump-wasm? dump-wasm?
                        #:opts opts))

(define* (read-and-compile port #:key
                           (import-abi? #f)
                           (export-abi? #t)
                           (from (current-language))
                           (env (default-environment from))
                           (optimization-level (default-optimization-level))
                           (warning-level (default-warning-level))
                           (dump-cps? #f)
                           (dump-wasm? #f)
                           (opts '()))
  ;; FIXME: Right now the tree-il->cps phase will expand primitives to
  ;; Guile VM primitives, e.g. including `heap-object?` and so on.  We
  ;; need to instead expand into more wasm-appropriate primitives, at
  ;; some point anyway.
  (define cps
    (%read-and-compile port #:env env #:from from #:to 'cps
                       #:optimization-level optimization-level
                       #:warning-level warning-level))
  (high-level-cps->wasm cps
                        #:import-abi? import-abi?
                        #:export-abi? export-abi?
                        #:env env
                        #:optimization-level optimization-level
                        #:warning-level warning-level
                        #:dump-cps? dump-cps?
                        #:dump-wasm? dump-wasm?
                        #:opts opts))

(define* (compile-file input-file #:key
                       (output-file (error "missing output file"))
                       (import-abi? #f)
                       (export-abi? #t)
                       (from (current-language))
                       (env (default-environment from))
                       (optimization-level (default-optimization-level))
                       (warning-level (default-warning-level))
                       (dump-cps? #f)
                       (dump-wasm? #f)
                       (opts '()))
  (call-with-input-file input-file
    (lambda (in)
      (set-port-encoding! in (or (file-encoding in) "UTF-8"))
      (let ((wasm (read-and-compile in
                                    #:import-abi? import-abi?
                                    #:export-abi? export-abi?
                                    #:from from
                                    #:env env
                                    #:optimization-level optimization-level
                                    #:warning-level warning-level
                                    #:dump-cps? dump-cps?
                                    #:dump-wasm? dump-wasm?
                                    #:opts opts)))
        (let ((bytes (assemble-wasm wasm)))
          (call-with-output-file output-file
            (lambda (out)
              (put-bytevector out bytes))))))))
