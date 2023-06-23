;;; Standard library for Hoot runtime
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
;;; Standard runtime routines for Hoot WebAssembly runtime.
;;;
;;; Code:

(define-module (hoot stdlib)
  #:use-module (ice-9 match)
  #:use-module (wasm types)
  #:export (compute-stdlib))

(define void-block-type (make-type-use #f (make-func-sig '() '())))
(define i32-block-type (make-type-use #f (make-func-sig '() '(i32))))

(define scm-type (make-ref-type #f 'eq))

(define kvarargs-sig
  (make-func-sig (list (make-param '$nargs 'i32)
                       (make-param '$arg0 scm-type)
                       (make-param '$arg1 scm-type)
                       (make-param '$arg2 scm-type))'()))

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
                                 #f
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
         (struct ($heap-number $heap-object))
         (struct ($bignum $heap-number)
                 ($val (make-ref-type #f 'extern)))
         (struct ($flonum $heap-number)
                 ($val 'f64))
         (struct ($complex $heap-number)
                 ($real 'f64)
                 ($imag 'f64))
         (struct ($fraction $heap-number)
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
           `((local.get 0) (call $string-hash) (call $finish-heap-object-hash)
             (i32.const 0) (local.get 0) (struct.new $string)
             (struct.new $symbol)
             (call $intern-symbol!)))
          (make-func
           '$intern-symbol!
           (make-type-use #f (make-func-sig
                              (list (make-param '$sym
                                                (make-ref-type #f '$symbol)))
                              (list (make-ref-type #f '$symbol))))
           '()
           ;; FIXME: Actually interning into symtab is unimplemented!
           `((local.get 0)))
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
             (struct.new $keyword)))
          (make-func
           '$grow-raw-stack
           (make-type-use #f (make-func-sig (list (make-param '$sp 'i32)) '()))
           '()
           ;; Grow the stack by at least 50% and at least the needed
           ;; space.  Trap if we fail to grow.
           ;; additional_size = (current_size >> 1) | needed_size
           `((memory.size $raw-stack)
             (i32.const 1)
             (i32.shr_u)
             (local.get $sp)
             (i32.const 16) ;; Wasm pages are 64 kB.
             (i32.shr_u)
             (i32.or)
             (memory.grow $raw-stack)
             (i32.const -1)
             (i32.eq)
             (if #f ,void-block-type ((unreachable)) ())))
          (make-func
           '$grow-scm-stack
           (make-type-use #f (make-func-sig (list (make-param '$sp 'i32)) '()))
           '()
           ;; Grow as in $grow-raw-stack.
           `((i32.const 0)
             (i31.new)
             (table.size $scm-stack)
             (i32.const 1)
             (i32.shr_u)
             (local.get $sp)
             (i32.or)
             (table.grow $scm-stack)
             (i32.const -1)
             (i32.eq)
             (if #f ,void-block-type ((unreachable)) ())))
          (make-func
           '$invalid-continuation
           (make-type-use '$kvarargs kvarargs-sig)
           '()
           '((unreachable)))
          (make-func
           '$grow-ret-stack
           (make-type-use #f (make-func-sig (list (make-param '$sp 'i32)) '()))
           '()
           ;; Grow as in $grow-raw-stack.
           `((ref.func $invalid-continuation)
             (table.size $ret-stack)
             (i32.const 1)
             (i32.shr_u)
             (local.get $sp)
             (i32.or)
             (table.grow $ret-stack)
             (i32.const -1)
             (i32.eq)
             (if #f ,void-block-type ((unreachable)) ())))
          (make-func
           '$slow-<
           (make-type-use #f (make-func-sig (list (make-param '$a scm-type)
                                                  (make-param '$b scm-type))
                                            '(i32)))
           '()
           `((unreachable)))
          (make-func
           '$slow-<=
           (make-type-use #f (make-func-sig (list (make-param '$a scm-type)
                                                  (make-param '$b scm-type))
                                            '(i32)))
           '()
           `((unreachable)))
          (make-func
           '$slow-=
           (make-type-use #f (make-func-sig (list (make-param '$a scm-type)
                                                  (make-param '$b scm-type))
                                            '(i32)))
           '()
           `((unreachable)))))

  ;; Because V8 and binaryen don't really support non-nullable table
  ;; types right now, we currently use nullable tables.  Grr.
  (define tables
    (list (make-table '$argv
                      (make-table-type
                       (make-limits 0 #f)
                       (make-ref-type #t 'eq))
                      #f)
          (make-table '$scm-stack
                      (make-table-type
                       (make-limits 0 #f)
                       (make-ref-type #t 'eq))
                      #f)
          (make-table '$ret-stack
                      (make-table-type
                       (make-limits 0 #f)
                       (make-ref-type #t '$kvarargs))
                      #f)))

  (define memories
    (list (make-memory '$raw-stack
                       (make-mem-type (make-limits 0 #f)))))

  (define globals
    (let ((scm-init '((i32.const 0) i31.new)))
      (list (make-global '$arg3 (make-global-type #t scm-type) scm-init)
            (make-global '$arg4 (make-global-type #t scm-type) scm-init)
            (make-global '$arg5 (make-global-type #t scm-type) scm-init)
            (make-global '$arg6 (make-global-type #t scm-type) scm-init)
            (make-global '$arg7 (make-global-type #t scm-type) scm-init)
            (make-global '$ret-sp (make-global-type #t 'i32) '((i32.const 0)))
            (make-global '$scm-sp (make-global-type #t 'i32) '((i32.const 0)))
            (make-global '$raw-sp (make-global-type #t 'i32) '((i32.const 0))))))

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
                  (map (match-lambda
                        (($ <memory> id type)
                         (make-import "abi" (symbol->string id) 'memory id
                                      type)))
                       memories)
                  imports)
                 imports)
             funcs
             (if import-abi? '() tables)
             (if import-abi? '() memories)
             (if import-abi? '() globals)
             exports start elems datas tags strings custom))
