;;; WebAssembly VM
;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
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
;;; Virtual machine for WebAssembly.
;;;
;;; Code:

(define-module (wasm vm)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (wasm parse)
  #:use-module (wasm stack)
  #:use-module (wasm types)
  #:export (validate-wasm
            load-and-validate-wasm
            validated-wasm?
            validated-wasm-ref

            make-wasm-global
            wasm-global?
            wasm-global-ref
            wasm-global-set!
            wasm-global-mutable?

            make-wasm-memory
            wasm-memory?
            wasm-memory-bytes
            wasm-memory-size
            wasm-memory-limits
            wasm-memory-grow!

            make-wasm-table
            wasm-table?
            wasm-table-size
            wasm-table-ref
            wasm-table-set!
            wasm-table-fill!
            wasm-table-copy!
            wasm-table-init!
            wasm-table-grow!

            wasm-stack?
            wasm-stack-items

            wasm-func?
            wasm-null?
            wasm-struct?
            wasm-array?

            instantiate-wasm
            wasm-instance?
            wasm-instance-module
            wasm-instance-export-ref
            wasm-instance-export-names

            current-instruction-listener

            &wasm-error

            &wasm-validation-error
            wasm-validation-error?
            wasm-validation-error-module

            &wasm-instance-error
            wasm-instance-error?

            &wasm-runtime-error
            wasm-runtime-error?
            wasm-runtime-error-position
            wasm-runtime-error-instruction
            wasm-runtime-error-instance
            wasm-runtime-error-stack
            wasm-runtime-error-blocks
            wasm-runtime-error-locals))


;;;
;;; Types
;;;

(define (s32? x)
  (and (exact-integer? x) (< (- (ash -1 31) 1) x (ash 1 31))))

(define (s31? x)
  (and (exact-integer? x) (< (- (ash -1 30) 1) x (ash 1 30))))

(define (u32? x)
  (and (exact-integer? x) (< -1 x (- (ash 1 32) 1))))

(define (s8->u8 x)
  (logand x #xff))

(define (s8-overflow x)
  (centered-remainder x (ash 1 8)))

(define (s16->u16 x)
  (logand x #xffff))

(define (s16-overflow x)
  (centered-remainder x (ash 1 16)))

(define (s32->u32 x)
  (logand x #xffffFFFF))

(define (s31->u31 x)
  (logand x #x7fffFFFF))

(define (s32-overflow x)
  (centered-remainder x (ash 1 32)))

(define (s64? x)
  (and (exact-integer? x) (< (- (ash -1 63) 1) x (ash 1 63))))

(define (u64? x)
  (and (exact-integer? x) (< -1 x (- (ash 1 64) 1))))

(define (s64->u64 x)
  (logand x #xffffFFFFffffFFFF))

(define (s64-overflow x)
  (centered-remainder x (ash 1 64)))

(define (f32? x)
  (and (number? x) (or (inexact? x) (exact-integer? x))))

(define (f64? x)
  (and (number? x) (or (inexact? x) (exact-integer? x))))

(define (resolve-type type)
  (match type
    (($ <sub-type> _ _ type) type)
    (_ type)))


;;;
;;; Validation
;;;

(define-record-type <validated-wasm>
  (make-validated-wasm wasm)
  validated-wasm?
  (wasm validated-wasm-ref))

(define-exception-type &wasm-error &error
  make-wasm-error
  wasm-error?)

(define-exception-type &wasm-validation-error &wasm-error
  make-wasm-validation-error
  wasm-validation-error?
  (wasm wasm-validation-error-wasm))

;; TODO: Trace instruction position within function to give context to
;; validation errors.
(define (validate-wasm wasm)
  (define types
    (list->vector
     (append-map (match-lambda
                   (($ <rec-group> (($ <type> _ types) ...)) types)
                   (($ <type> _ type) (list type)))
                 (wasm-types wasm))))
  (define global-types
    (list->vector
     (append (filter-map (match-lambda
                           (($ <import> _ _ 'global _ type) type)
                           (_ #f))
                         (wasm-imports wasm))
             (map global-type (wasm-globals wasm)))))
  (define func-sigs
    (list->vector
     (append (filter-map
              (match-lambda
                (($ <import> _ _ 'func _ ($ <type-use> _ sig))
                 sig)
                (_ #f))
              (wasm-imports wasm))
             (map (match-lambda
                    (($ <func> _ ($ <type-use> _ sig))
                     sig))
                  (wasm-funcs wasm)))))
  (define memories
    (list->vector
     (append (filter-map (match-lambda
                           (($ <import> _ _ 'memory _ type) type)
                           (_ #f))
                         (wasm-imports wasm))
             (wasm-memories wasm))))
  (define tables
    (list->vector
     (append (filter-map (match-lambda
                           (($ <import> _ _ 'table _ type) type)
                           (_ #f))
                         (wasm-imports wasm))
             (map table-type (wasm-tables wasm)))))
  (define elems (list->vector (wasm-elems wasm)))
  (define strings (list->vector (wasm-strings wasm)))
  (define (validation-error msg . irritants)
    (raise-exception
     (make-exception
      (make-wasm-validation-error wasm)
      (make-exception-with-message
       (format #f "WASM validation error: ~a" msg))
      (make-exception-with-irritants irritants))))
  (define (assert-s32 x)
    (unless (s32? x)
      (validation-error "i32 constant out of range" x)))
  (define (assert-s64 x)
    (unless (s64? x)
      (validation-error "i64 constant out of range" x)))
  (define (assert-f32 x)
    (unless (f32? x)
      (validation-error "f32 constant out of range" x)))
  (define (assert-f64 x)
    (unless (f64? x)
      (validation-error "f64 constant out of range" x)))
  (define (check-vector vec id msg)
    (unless (< -1 id (vector-length vec))
      (validation-error msg id)))
  (define (check-func id)
    (check-vector func-sigs id "invalid function"))
  (define (check-memory id)
    (check-vector memories id "invalid memory"))
  (define (check-table id)
    (check-vector tables id "invalid table"))
  (define (check-elem id)
    (check-vector elems id "invalid element"))
  (define (check-string id)
    (check-vector strings id "invalid string"))
  (define (validate-const type instrs)
    (define (validate-instr ctx instr)
      (match (apply-stack-effect ctx (compute-stack-effect ctx instr))
        (($ <invalid-ctx> reason)
         (validation-error reason instr))
        (ctx
         (match instr
           (('i32.const x) (assert-s32 x))
           (('i64.const x) (assert-s64 x))
           (('f32.const x) (assert-f32 x))
           (('f64.const x) (assert-f64 x))
           (('string.const idx) (check-string idx))
           (('ref.func f) (check-func f))
           (('ref.null _) #t)
           (_ (validation-error "invalid constant instruction" instr)))
         ctx)))
    ;; We need to make a phony func object that represents the
    ;; expected result type of the constant instructions.
    (let* ((sig (make-func-sig '() (list type)))
           (func (make-func #f (make-type-use #f sig) '() '())))
      (let loop ((ctx (initial-ctx wasm func))
                 (instrs instrs))
        (match instrs
          (() #t)
          ((instr . rest)
           (validate-instr ctx instr))))))
  (define (validate-global global)
    (match global
      (($ <global> _ ($ <global-type> _ type) instrs)
       (validate-const type instrs))))
  (define (validate-data data)
    (match data
      (($ <data> _ _ _ offset _)
       (when offset
         (validate-const 'i32 offset)))))
  (define (validate-elem elem)
    (match elem
      (($ <elem> _ mode _ type offset inits)
       (when (eq? mode 'active)
         (validate-const 'i32 offset))
       (for-each (lambda (init)
                   (validate-const type init))
                 inits))))
  (define (validate-func func)
    (match func
      (($ <func> _ ($ <type-use> _ type) _ body)
       (define (lookup-block-type bt)
         (match bt
           (#f (make-func-sig '() '()))
           ((? exact-integer? idx) (type-val (list-ref (wasm-types wasm) idx)))
           (($ <type-use> _ sig) sig)
           (type (make-func-sig '() (list type)))))
       (define (push-block* ctx bt loop?)
         (match bt
           (($ <func-sig> (($ <param> _ params) ...) results)
            (push-block ctx #f params results #:is-loop? loop?))))
       (define (defaultable-type? type)
         (match type
           ((or 'i8 'i16 'i32 'i64 'f32 'f64) #t)
           (($ <ref-type> #t) #t)
           (_ #f)))
       (define (validate-instr ctx instr)
         (match (apply-stack-effect ctx (compute-stack-effect ctx instr))
           (($ <invalid-ctx> reason)
            (validation-error reason instr))
           (ctx
            (match instr
              (('if _ (= lookup-block-type bt) consequent alternate)
               (validate-branch (push-block* ctx bt #f) consequent)
               (validate-branch (push-block* ctx bt #f) alternate))
              (('block _ (= lookup-block-type bt) body)
               (validate-branch (push-block* ctx bt #f) body))
              (('loop _ (= lookup-block-type bt) body)
               (validate-branch (push-block* ctx bt #t) body))
              (('i32.const x) (assert-s32 x))
              (('i64.const x) (assert-s64 x))
              (('f32.const x) (assert-f32 x))
              (('f64.const x) (assert-f64 x))
              (('string.const idx) (check-string idx))
              (('global.set idx)
               (match (vector-ref global-types idx)
                 (($ <global-type> mutable? _)
                  (unless mutable?
                    (validation-error "global is immutable" idx)))))
              (((or 'memory.size 'memory.grow) id)
               (check-memory id))
              (((or 'i32.load 'i32.load8_s 'i32.load8_u
                    'i32.load16_s 'i32.load16_u
                    'i64.load 'i64.load8_s 'i64.load8_u
                    'i64.load16_s 'i64.load16_u 'i64.load32_s 'i64.load32_u
                    'i32.store 'i32.store8 'i32.store16
                    'i64.store 'i64.store8 'i64.store16 'i64.store32)
                ($ <mem-arg> id _ _))
               (check-memory id))
              (((or 'table.set 'table.get 'table.size
                    'table.grow 'table.fill) table)
               (check-table table))
              (('table.copy dst src)
               (check-table dst)
               (check-table src))
              (('table.init table elem)
               (check-table table)
               (check-elem elem))
              (('elem.drop table) (check-elem table))
              (('ref.func f) (check-func f))
              (('struct.new_default t)
               ;; Validate that all fields have defaults.
               (match (resolve-type (vector-ref types t))
                 ((and struct ($ <struct-type> (($ <field> _ _ types) ...)))
                  (unless (every defaultable-type? types)
                    (validation-error "struct type has non-defaultable fields"
                                      struct)))))
              (('struct.set t i)
               (let* ((type (resolve-type (vector-ref types t)))
                      (field (list-ref (struct-type-fields type) i)))
                 (unless (field-mutable? field)
                   (validation-error "struct field is immutable" type field instr))))
              (('array.new_default t)
               (let ((array (resolve-type (vector-ref types t))))
                 (unless (defaultable-type? (array-type-type array))
                   (validation-error "array type has non-defaultable element type"
                                     array))))
              ((or ('array.set t)
                   ('array.init_data t _)
                   ('array.init_elem t _))
               (let ((type (resolve-type (vector-ref types t))))
                 (unless (array-type-mutable? type)
                   (validation-error "array is immutable" type instr))))
              ;; TODO: Validate array.get_s, ref.cast, etc.
              (_ #t))
            ctx)))
       (define (validate-branch ctx instrs)
         (let loop ((ctx ctx)
                    (instrs* instrs))
           (match instrs*
             (()
              (match (fallthrough ctx)
                (($ <invalid-ctx> reason)
                 (validation-error reason instrs))
                (_ #t)))
             ((instr . rest)
              (loop (validate-instr ctx instr) rest)))))
       (validate-branch (initial-ctx wasm func) body))))
  (for-each validate-global (wasm-globals wasm))
  (for-each validate-func (wasm-funcs wasm))
  (for-each validate-data (wasm-datas wasm))
  (for-each validate-elem (wasm-elems wasm))
  (make-validated-wasm wasm))

(define (load-and-validate-wasm obj)
  "Load and validate the WASM module within OBJ.  OBJ may be a <wasm>
record produced by 'resolve-wasm', a bytevector containing a WASM
binary, or an input port from which a WASM binary is read."
  (validate-wasm
   (cond
    ((wasm? obj) obj)
    ((bytevector? obj)
     (call-with-input-bytevector obj parse-wasm))
    ((port? obj)
     (parse-wasm obj))
    (else
     (error "not a WASM object" obj)))))


;;;
;;; Instances
;;;

(define-exception-type &wasm-runtime-error &wasm-error
  make-wasm-runtime-error
  wasm-runtime-error?
  (instruction wasm-runtime-error-instruction)
  (position wasm-runtime-error-position)
  (instance wasm-runtime-error-instance)
  (stack wasm-runtime-error-stack)
  (blocks wasm-runtime-error-blocks)
  (locals wasm-runtime-error-locals))

;; TODO: Use a vector instead of a list to avoid allocation for each
;; push.  Maximum stack depth for each function can be determined at
;; validation time.
(define-record-type <wasm-stack>
  (%make-wasm-stack items)
  wasm-stack?
  (items wasm-stack-items set-wasm-stack-items!))

(define (make-wasm-stack)
  (%make-wasm-stack '()))

(define (stack-push! stack x)
  (set-wasm-stack-items! stack (cons x (wasm-stack-items stack))))

(define (stack-push-all! stack vals)
  (for-each (lambda (val)
              (stack-push! stack val))
            vals))

(define (stack-peek stack)
  (match (wasm-stack-items stack)
    (() #f)
    ((head . _)
     head)))

(define (stack-pop! stack)
  (match (wasm-stack-items stack)
    (() #f)
    ((head . rest)
     (set-wasm-stack-items! stack rest)
     head)))

(define (stack-pop-n! stack n)
  (let loop ((n n)
             (result '()))
    (if (= n 0)
        result
        (loop (- n 1) (cons (stack-pop! stack) result)))))

;; TODO: Replace with global weak hash table that maps procedures to
;; signatures.
(define-record-type <wasm-func>
  (make-wasm-func proc sig)
  wasm-func?
  (proc wasm-func-proc)
  (sig wasm-func-sig))

(set-record-type-printer! <wasm-func>
                          (lambda (f port)
                            (format port "#<wasm-func ~s>"
                                    (wasm-func-proc f))))

(define-record-type <wasm-global>
  (make-wasm-global value mutable?)
  wasm-global?
  (value wasm-global-ref %wasm-global-set!)
  (mutable? wasm-global-mutable?))

(define (wasm-global-set! global val)
  (if (wasm-global-mutable? global)
      (%wasm-global-set! global val)
      (error "WASM global is immutable" global)))

(define %page-size (* 64 1024))
(define %max-pages (/ (ash 1 32) %page-size))

(define (clamp-to-limits x limits default-max)
  (match limits
    (($ <limits> min max)
     (let ((max* (or max default-max)))
       (cond
        ((< x min) min)
        ((> x max*) max*)
        (else x))))))

(define-record-type <wasm-memory>
  (%make-wasm-memory bytes size limits)
  wasm-memory?
  (bytes wasm-memory-bytes set-wasm-memory-bytes!)
  (size wasm-memory-size set-wasm-memory-size!)
  (limits wasm-memory-limits))

(define (make-bytevector/pages n)
  (make-bytevector (* n %page-size) 0))

(define* (make-wasm-memory size #:optional (limits (make-limits 1 #f)))
  (let ((size* (clamp-to-limits size limits %max-pages)))
    (%make-wasm-memory (make-bytevector/pages size*) size* limits)))

(define (wasm-memory-grow! memory n)
  (match memory
    (($ <wasm-memory> old-bytes old-size limits)
     (if (= n 0)
         old-size
         (let ((new-size (clamp-to-limits (+ (wasm-memory-size memory) n)
                                          limits %max-pages)))
           (if (= new-size old-size)
               -1
               (let ((new-bytes (make-bytevector/pages new-size)))
                 (bytevector-copy! old-bytes 0 new-bytes 0
                                   (* old-size %page-size))
                 (set-wasm-memory-bytes! memory new-bytes)
                 (set-wasm-memory-size! memory new-size)
                 old-size)))))))

(define-record-type <wasm-table>
  (%make-wasm-table elements limits)
  wasm-table?
  (elements wasm-table-elements set-wasm-table-elements!)
  (limits wasm-table-limits))

(define %max-table-size (- (ash 1 32) 1))

(define* (make-wasm-table size #:optional (limits (make-limits 1 #f)))
  (let ((size* (clamp-to-limits size limits %max-table-size)))
    (%make-wasm-table (make-vector size*) limits)))

(define (wasm-table-size table)
  (vector-length (wasm-table-elements table)))

(define (wasm-table-ref table i)
  (vector-ref (wasm-table-elements table) i))

(define (wasm-table-set! table i x)
  (vector-set! (wasm-table-elements table) i x))

(define (wasm-table-fill! table start fill length)
  (vector-fill! (wasm-table-elements table) fill start (+ start length)))

(define (wasm-table-copy! table at src start length)
  (vector-copy! (wasm-table-elements table) at (wasm-table-elements src)
                start (+ start length)))

(define (wasm-table-init! table at elems start length)
  (vector-copy! (wasm-table-elements table) at elems start (+ start length)))

(define (wasm-table-grow! table n init)
  (match table
    (($ <wasm-table> elems limits)
     (let ((old-size (vector-length elems)))
       (if (= n 0)
           old-size
           (let ((new-size (clamp-to-limits (+ old-size n) limits %max-table-size)))
             (if (= new-size old-size)
                 -1
                 (let ((new-elems (make-vector new-size)))
                   (vector-copy! new-elems 0 elems)
                   (do ((i old-size (+ i 1)))
                       ((= i new-size))
                     (vector-set! new-elems i init))
                   (set-wasm-table-elements! table new-elems)
                   old-size))))))))

(define-record-type <wasm-null>
  (make-wasm-null type)
  wasm-null?
  (type wasm-null-type))

;; TODO: Packed fields.
(define-record-type <wasm-struct>
  (%make-wasm-struct type fields)
  wasm-struct?
  (type wasm-struct-type)
  (fields wasm-struct-fields))

(set-record-type-printer! <wasm-struct>
                          (lambda (struct port)
                            (format port "#<wasm-struct ~s>"
                                    (wasm-struct-fields struct))))

(define (make-wasm-struct type fields)
  (%make-wasm-struct type (list->vector fields)))

(define (wasm-struct-ref struct field)
  (vector-ref (wasm-struct-fields struct) field))

(define (wasm-struct-type-fields struct)
  (struct-type-fields (resolve-type (wasm-struct-type struct))))

(define (wasm-struct-ref-unsigned struct field)
  (let ((x (wasm-struct-ref struct field)))
    (match (field-type (list-ref (wasm-struct-type-fields struct) field))
      ('i8 (s8->u8 x))
      ('i16 (s16->u16 x)))))

(define (wasm-struct-ref-signed struct field)
  (let ((x (wasm-struct-ref struct field)))
    (match (field-type (list-ref (wasm-struct-type-fields struct) field))
      ('i8 (s8-overflow x))
      ('i16 (s16-overflow x)))))

(define (wasm-struct-set! struct field value)
  (vector-set! (wasm-struct-fields struct) field value))

;; TODO: Use bytevectors for packed arrays.
(define-record-type <wasm-array>
  (%make-wasm-array type vector)
  wasm-array?
  (type wasm-array-type)
  (vector wasm-array-vector))

(set-record-type-printer! <wasm-array>
                          (lambda (array port)
                            (format port "#<wasm-array ~s>"
                                    (wasm-array-vector array))))

(define* (make-wasm-array type k #:optional (fill *unspecified*))
  (%make-wasm-array type (make-vector k fill)))

(define (wasm-array-length array)
  (vector-length (wasm-array-vector array)))

(define (wasm-array-element-type array)
  (array-type-type (resolve-type (wasm-array-type array))))

(define (wasm-array-ref array i)
  (vector-ref (wasm-array-vector array) i))

(define (wasm-array-ref-unsigned array i)
  (let ((x (wasm-array-ref array i)))
    (match (wasm-array-element-type array)
      ('i8 (s8->u8 x))
      ('i16 (s16->u16 x)))))

(define (wasm-array-ref-signed array i)
  (let ((x (wasm-array-ref array i)))
    (match (wasm-array-element-type array)
      ('i8 (s8-overflow x))
      ('i16 (s16-overflow x)))))

(define (wasm-array-set! array i value)
  (vector-set! (wasm-array-vector array) i value))

(define (wasm-array-init-data! array at data offset length)
  (let ((ref (match (array-type-type (wasm-array-type array))
               ('f64 f64vector-ref)
               ('f32 f32vector-ref)
               ('i32 s32vector-ref)
               ('i16 u16vector-ref)
               ('i8 u8vector-ref)
               (type (error "non-numeric array type" type)))))
    (do ((i 0 (+ i 1)))
        ((= i length))
      (wasm-array-set! array (+ i at) (ref data (+ i offset))))))

(define (wasm-array-init-elem! array at elem offset length)
  (do ((i 0 (+ i 1)))
        ((= i length))
      (wasm-array-set! array (+ i at) (vector-ref elem (+ i offset)))))

(define (wasm-array-fill! array start fill length)
  (vector-fill! (wasm-array-vector array) fill start (+ start length)))

(define (wasm-array-copy! dst at src start length)
  (vector-copy! (wasm-array-vector dst) at
                (wasm-array-vector src) start (+ start length)))

(define (wasm-array->string array start end)
  (list->string
   (let loop ((i start))
     (if (= i end)
         '()
         (cons (integer->char (wasm-array-ref-unsigned array i))
               (loop (+ i 1)))))))

(define (wasm-array-encode-string! array str start)
  (let ((utf8 (string->utf8 str)))
    (do ((i 0 (+ i 1)))
        ((= i (bytevector-length utf8)))
      (wasm-array-set! array (+ i start) (bytevector-u8-ref utf8 i)))))

(define-record-type <wasm-string-iterator>
  (make-wasm-string-iterator string index)
  wasm-string-iterator?
  (string wasm-string-iterator-string)
  (index wasm-string-iterator-index set-wasm-string-iterator-index!))

(define (wasm-string-iterator-next! iter)
  (match iter
    (($ <wasm-string-iterator> str i)
     (let ((len (string-length str)))
       (set-wasm-string-iterator-index! iter (+ i 1))
       (if (>= i len)
           -1
           (char->integer (string-ref str i)))))))

(define (wasm-string-iterator-advance! iter k)
  (match iter
    (($ <wasm-string-iterator> str i)
     (let ((len (string-length str)))
       (set-wasm-string-iterator-index! iter (+ i k))
       (max (- (min (+ i k) len) i) 0)))))

;; A bit of global state for ref type canonicalization across modules.
(define *canonical-groups* (make-hash-table))

(define (canonicalize-types! types)
  ;; Create a vector big enough to hold all of the resulting types.
  (let ((canonical-vec (make-vector
                        (fold (lambda (type sum)
                                (match type
                                  (($ <rec-group> types)
                                   (+ sum (length types)))
                                  (_ (+ sum 1))))
                              0 types))))
    (define (visit-group types group-start)
      ;; Rolling up a type replaces indices outside of the type group
      ;; with a canonical type descriptor and indices inside of the
      ;; type group with relative indices.  This creates a new type
      ;; that can be equal? tested against a type from another module.
      (define (roll-up type group-start)
        (match type
          ((? symbol?) type)
          ((? exact-integer? idx)
           (if (< type group-start)
               `(outer ,(vector-ref canonical-vec idx))
               (- idx group-start)))
          (($ <ref-type> nullable? heap-type)
           (make-ref-type nullable? (roll-up heap-type group-start)))
          (($ <func-sig> params results)
           (make-func-sig (map (match-lambda
                                 (($ <param> _ type)
                                  (make-param #f (roll-up type group-start))))
                               params)
                          (map (lambda (type)
                                 (roll-up type group-start))
                               results)))
          (($ <struct-type> fields)
           (make-struct-type
            (map (match-lambda
                   (($ <field> _ mutable? type)
                    (make-field #f mutable? (roll-up type group-start))))
                 fields)))
          (($ <array-type> mutable? type)
           (make-array-type mutable? (roll-up type group-start)))
          (($ <sub-type> final? supers type)
           (make-sub-type final?
                          (map (lambda (super)
                                 (roll-up super group-start))
                               supers)
                          (roll-up type group-start)))))
      ;; If a type group with identical structure has already been
      ;; canonicalized, return the cached type descriptors.  Otherwise,
      ;; generate new ones, cache them, and return them.
      (let ((types* (map (lambda (t) (roll-up t group-start)) types)))
        (match (hash-ref *canonical-groups* types*)
          ;; Cache hit: Just copy 'em over.
          ((? vector? cached-group)
           (do ((i 0 (+ i 1)))
               ((= i (vector-length cached-group)))
             (vector-set! canonical-vec (+ group-start i)
                          (vector-ref cached-group i)))
           (+ group-start (vector-length cached-group)))
          ;; Cache miss: Generate and cache new descriptors.
          (#f
           (let ((group-vec (make-vector (length types))))
             (let loop ((types types*)
                        (i 0))
               ;; Unrolling a type replaces relative recursive type
               ;; indices with canonical type references.
               (define (unroll type)
                 (match type
                   ((? symbol?) type)
                   ;; Types may have recursive references to other
                   ;; types within the same group, so we're lazy about
                   ;; it.
                   ((? exact-integer? idx)
                    (delay (vector-ref group-vec idx)))
                   ;; Types from outside the group are already
                   ;; unrolled so recursion stops.
                   (('outer type) type)
                   (($ <ref-type> nullable? heap-type)
                    (make-ref-type nullable? (unroll heap-type)))
                   (($ <func-sig> params results)
                    (make-func-sig (map (match-lambda
                                          (($ <param> _ type)
                                           (make-param #f (unroll type))))
                                        params)
                                   (map unroll results)))
                   (($ <struct-type> fields)
                    (make-struct-type
                     (map (match-lambda
                            (($ <field> _ mutable? type)
                             (make-field #f mutable? (unroll type))))
                          fields)))
                   (($ <array-type> mutable? type)
                    (make-array-type mutable? (unroll type)))
                   (($ <sub-type> final? supers type)
                    (make-sub-type final?
                                   (map unroll supers)
                                   (unroll type)))))
               (match types
                 (()
                  (hash-set! *canonical-groups* types* group-vec)
                  (+ group-start i))
                 ((type . rest)
                  (let ((desc (unroll type)))
                    (vector-set! group-vec i desc)
                    (vector-set! canonical-vec (+ group-start i) desc)
                    (loop rest (+ i 1)))))))))))
    ;; Visit all the type groups and canonicalize them.  A type that
    ;; is not in a recursive type group is treated as being in a group
    ;; of one.
    (let loop ((groups types)
               (i 0))
      (match groups
        (() #t)
        ((($ <rec-group> (($ <type> _ types) ...)) . rest)
         (loop rest (visit-group types i)))
        ((($ <type> _ type) . rest)
         (loop rest (visit-group (list type) i)))))
    canonical-vec))

(define (default-for-type type)
  (match type
    ((or 'i8 'i16 'i32 'i64) 0)
    ((or 'f32 'f64) 0.0)
    ((or (? ref-type?) (? struct-type?) (? array-type?) (? sub-type?))
     (make-wasm-null type))))

(define (is-a? x type)
  (match type
    ('i32 (s32? x))
    ('i64 (s64? x))
    ('f32 (f32? x))
    ('f64 (f64? x))
    ('i31 (s31? x))
    ('extern #t)
    ('func (wasm-func? x))
    ((? func-sig?)
     (and (wasm-func? x) (eq? (wasm-func-sig x) type)))
    ((or 'eq 'any)
     (or (s31? x) (wasm-struct? x) (wasm-array? x)))
    ('i31 (s31? x))
    ('struct (wasm-struct? x))
    ('array (wasm-array? x))
    ('string (string? x))
    (($ <ref-type> _ heap-type)
     (is-a? x heap-type))
    (_
     (let loop ((x-type (cond
                         ((wasm-func? x) (wasm-func-sig x))
                         ((wasm-struct? x) (wasm-struct-type x))
                         ((wasm-array? x) (wasm-array-type x))
                         (else #f))))
       (match x-type
         (#f #f)
         ((and x-type ($ <sub-type> _ x-supers _))
          (or (eq? x-type type)
              (any (match-lambda
                     ((? promise? x-super) (loop (force x-super)))
                     (x-super (loop x-super)))
                   x-supers)))
         (x-type (eq? x-type type)))))))

;; Some more global state that keeps a record of all exported WASM
;; functions so that we can avoid runtime type checking when making
;; calls directly from one instance to another.
(define *exported-functions* (make-weak-key-hash-table))

(define (register-exported-function! wrap func)
  (hashq-set! *exported-functions* wrap func))

(define (lookup-exported-function wrap)
  (hashq-ref *exported-functions* wrap))

(define-record-type <wasm-instance>
  (make-wasm-instance module types globals funcs memories tables datas elems
                      strings exports)
  wasm-instance?
  (module wasm-instance-module)
  (types wasm-instance-types)
  (globals wasm-instance-globals)
  (funcs wasm-instance-funcs)
  (memories wasm-instance-memories)
  (tables wasm-instance-tables)
  (datas wasm-instance-datas)
  (elems wasm-instance-elems)
  (strings wasm-instance-strings)
  (exports wasm-instance-exports))

(set-record-type-printer! <wasm-instance>
                          (lambda (instance port)
                            (format port "#<wasm-instance ~a>"
                                    (object-address instance))))

(define-exception-type &wasm-instance-error &wasm-error
  make-wasm-instance-error
  wasm-instance-error?)

(define (instance-error msg . irritants)
  (raise-exception
   (make-exception
    (make-wasm-instance-error)
    (make-exception-with-message
     (format #f "WASM instantiation error: ~a" msg))
    (make-exception-with-irritants irritants))))

(define* (instantiate-wasm module #:key (imports '()))
  (define (lookup-import mod name)
    (assoc-ref (or (assoc-ref imports mod) '()) name))
  (match module
    (($ <validated-wasm>
        ($ <wasm> types wasm-imports funcs tables memories globals exports
                  start elems datas tags strings custom))
     (define (count-imports kind)
       (fold (lambda (i sum)
               (match i
                 (($ <import> _ _ k)
                  (if (eq? kind k) (+ sum 1) sum))))
             0 wasm-imports))
     (let* ((n-global-imports (count-imports 'global))
            (n-func-imports (count-imports 'func))
            (n-memory-imports (count-imports 'memory))
            (n-table-imports (count-imports 'table))
            (type-vec (canonicalize-types! types))
            (global-vec (make-vector (+ n-global-imports (length globals))))
            (func-vec (make-vector (+ n-func-imports (length funcs))))
            (memory-vec (make-vector (+ n-memory-imports (length memories))))
            (table-vec (make-vector (+ n-table-imports (length tables))))
            (data-vec (make-vector (length datas)))
            (elem-vec (make-vector (length elems)))
            (string-vec (list->vector strings))
            (export-table (make-hash-table))
            (instance (make-wasm-instance module type-vec global-vec func-vec
                                          memory-vec table-vec data-vec elem-vec
                                          string-vec export-table)))
       (define (type-check vals types)
         (unless (let loop ((vals vals)
                            (types types))
                   (match vals
                     (()
                      (match types
                        (() #t)
                        (_ #f)))
                     ((val . rest-vals)
                      (match types
                        (() #f)
                        ((type . rest-types)
                         (and (is-a? val type)
                              (loop rest-vals rest-types)))))))
           (error (format #f "type mismatch; expected ~a" types)
                  vals)))
       (define (convert-results vals types)
         (map (lambda (val type)
                (match type
                  ('i32
                   (match val
                     ((? s32?) val)
                     (#t 1)
                     (#f 0)
                     (_ (error "invalid i32" val))))
                  (_ val)))
              vals types))
       (define (make-import-closure mod name proc sig)
         (let ((result-types (func-sig-results sig)))
           (define (wrap . args)
             (call-with-values (lambda () (apply proc args))
               (lambda vals
                 (let ((vals* (convert-results vals result-types)))
                   (type-check vals* result-types)
                   (apply values vals*)))))
           (set-procedure-property! wrap 'name (format #f "~a:~a" mod name))
           wrap))
       (define (make-export-closure name func)
         (match func
           (($ <wasm-func> proc ($ <func-sig> (($ <param> _ param-types) ...)))
            (define (wrap . args)
              (type-check args param-types)
              (apply proc args))
            (set-procedure-property! wrap 'name (string->symbol name))
            wrap)))
       (define (instantiate-func idx func)
         (match func
           (($ <func> _ ($ <type-use> type-idx) locals body)
            (let* ((local-types (map local-type locals))
                   (sig (vector-ref type-vec type-idx))
                   (n-params (length (func-sig-params sig)))
                   (n-results (length (func-sig-results sig)))
                   (n-locals (length local-types))
                   (base-path `(,idx func)))
              (define (wasm-proc . args)
                (let ((stack (make-wasm-stack))
                      (locals (make-vector (+ n-params n-locals))))
                  ;; Initialize first n locals with args.
                  (let loop ((args args)
                             (i 0))
                    (match args
                      (() #t)
                      ((arg . rest)
                       (vector-set! locals i arg)
                       (loop rest (+ i 1)))))
                  ;; Initialize rest of locals with defaults.
                  (let loop ((types local-types)
                             (i n-params))
                    (match types
                      (() #t)
                      ((type . rest)
                       (vector-set! locals i (default-for-type type))
                       (loop rest (+ i 1)))))
                  ;; Execute the function body, handling early
                  ;; returns.  There are two classes of returns:
                  ;; return and return-call.  A regular return simply
                  ;; returns the top n values on the stack.  A
                  ;; return-call passes along a thunk to be tail
                  ;; called which will continue the computation.
                  (let ((tail-cont #f))
                    (call-with-block
                     (lambda (tag)
                       (execute* body base-path instance stack (list tag) locals))
                     (lambda (k)
                       (set! tail-cont k)))
                    (if tail-cont
                        (tail-cont)
                        (apply values (stack-pop-n! stack n-results))))))
              (make-wasm-func wasm-proc sig)))))
       (define (exec-init base-path init)
         (let ((stack (make-wasm-stack)))
           (execute* init (reverse base-path) instance stack '() #())
           (stack-pop! stack)))
       ;; Process imports.
       (let loop ((wasm-imports wasm-imports)
                  (global-idx 0)
                  (func-idx 0)
                  (memory-idx 0)
                  (table-idx 0))
         (match wasm-imports
           (() #t)
           ((($ <import> mod name 'func _ ($ <type-use> idx)) . rest)
            (let ((sig (vector-ref type-vec idx)))
              (match (lookup-import mod name)
                ((? procedure? proc)
                 ;; If proc is a wrapper around a WASM function from
                 ;; another instance, then we'll just use the internal
                 ;; procedure instead and skip unnecessary runtime type
                 ;; checking.
                 (match (lookup-exported-function proc)
                   ((and func ($ <wasm-func> _ other-sig))
                    (if (eq? sig other-sig)
                        (vector-set! func-vec func-idx func)
                        (instance-error "imported function signature mismatch"
                                        sig other-sig)))
                   (#f
                    (let ((wrap (make-import-closure mod name proc sig)))
                      (vector-set! func-vec func-idx (make-wasm-func wrap sig)))))
                 (loop rest global-idx (+ func-idx 1) memory-idx table-idx))
                (x (instance-error "invalid function import" mod name x)))))
           ((($ <import> mod name 'global _ type) . rest)
            (match (lookup-import mod name)
              ((? wasm-global? global)
               (vector-set! global-vec global-idx global)
               (loop rest (+ global-idx 1) func-idx memory-idx table-idx))
              (x (instance-error "invalid global import" mod name x))))
           ((($ <import> mod name 'memory _ type) . rest)
            (match (lookup-import mod name)
              ((? wasm-memory? memory)
               (vector-set! memory-vec memory-idx memory)
               (loop rest global-idx func-idx (+ memory-idx 1) table-idx))
              (x (instance-error "invalid memory import" mod name x))))
           ((($ <import> mod name 'table _ type) . rest)
            (match (lookup-import mod name)
              ((? wasm-table? table)
               (vector-set! table-vec table-idx table)
               (loop rest global-idx func-idx memory-idx (+ table-idx 1)))
              (x (instance-error "invalid table import" mod name x))))))
       ;; Initialize functions.
       (let loop ((funcs funcs)
                  (idx n-func-imports))
         (match funcs
           (() #t)
           ((func . rest)
            (vector-set! func-vec idx (instantiate-func idx func))
            (loop rest (+ idx 1)))))
       ;; Initialize globals.
       (let loop ((globals globals)
                  (idx n-global-imports))
         (match globals
           (() #t)
           (((and ($ <global> _ ($ <global-type> mutable? type) init) global) . rest)
            (let ((global (make-wasm-global (exec-init `(global ,idx) init) mutable?)))
              (vector-set! global-vec idx global))
            (loop rest (+ idx 1)))))
       ;; Initialize memories.
       (let loop ((memories memories)
                  (idx n-memory-imports))
         (match memories
           (() #t)
           ((($ <memory> _ ($ <mem-type> (and ($ <limits> min) limits))) . rest)
            (vector-set! memory-vec idx (make-wasm-memory min limits))
            (loop rest (+ idx 1)))))
       ;; Initialize data segments and copy active ones into memory.
       (let loop ((datas datas) (idx 0))
         (match datas
           (() #t)
           (((and ($ <data> _ mode mem-id instrs init) data) . rest)
            (vector-set! data-vec idx init)
            (when (eq? mode 'active)
              ;; Invoke the VM to process the constant
              ;; expressions that produce the offset value.
              (let ((offset (exec-init `(data ,idx) instrs))
                    (memory (vector-ref memory-vec mem-id)))
                (bytevector-copy! init 0 (wasm-memory-bytes memory)
                                  offset (bytevector-length init))))
            (loop rest (+ idx 1)))))
       ;; Initialize tables.
       (let loop ((tables tables)
                  (idx n-table-imports))
         (match tables
           (() #t)
           ((($ <table> _ ($ <table-type> (and ($ <limits> min) limits)) init) . rest)
            (vector-set! table-vec idx (make-wasm-table min limits))
            (loop rest (+ idx 1)))))
       ;; Initialize elements and copy active elements into tables.
       (let loop ((elems elems)
                  (idx 0))
         (match elems
           (() #t)
           (((and elem ($ <elem> _ mode table-idx type offset inits)) . rest)
            (let ((table (and table-idx (vector-ref table-vec table-idx)))
                  (offset (and (eq? mode 'active)
                               (exec-init `(elem ,idx 0) offset)))
                  (init-vec (list->vector
                             (let init-loop ((inits inits) (j 1))
                               (match inits
                                 (() '())
                                 ((instrs . rest)
                                  (cons (exec-init `(elem ,idx ,j) instrs)
                                        (init-loop rest (+ j 1)))))))))
              (vector-set! elem-vec idx init-vec)
              (when table
                (do ((i 0 (+ i 1)))
                    ((= i (vector-length init-vec)))
                  (wasm-table-set! table (+ offset i) (vector-ref init-vec i))))
              (loop rest (+ idx 1))))))
       ;; Call start function, if present.
       (when start ((wasm-func-proc (vector-ref func-vec start))))
       ;; Populate export table.
       (for-each (match-lambda
                   (($ <export> name 'func idx)
                    (let* ((func (vector-ref func-vec idx))
                           (wrap (make-export-closure name func)))
                      (register-exported-function! wrap func)
                      (hash-set! export-table name wrap)))
                   (($ <export> name 'global idx)
                    (hash-set! export-table name (vector-ref global-vec idx)))
                   (($ <export> name 'memory idx)
                    (hash-set! export-table name (vector-ref memory-vec idx)))
                   (($ <export> name 'table idx)
                    (hash-set! export-table name (vector-ref table-vec idx))))
                 exports)
       instance))))

(define (wasm-instance-export-ref instance name)
  (hash-ref (wasm-instance-exports instance) name))

(define (wasm-instance-export-names instance)
  (hash-fold (lambda (k v memo) (cons k memo))
             '() (wasm-instance-exports instance)))

(define (wasm-instance-global-ref instance idx)
  (vector-ref (wasm-instance-globals instance) idx))

(define (wasm-instance-drop-elem! instance idx)
  (vector-set! (wasm-instance-elems instance) idx #f))

;; Blocks are delimited by a prompt so that 'br', 'return',
;; 'return_call' and friends can abort to that prompt.
(define (call-with-block proc handler)
  (let ((tag (make-prompt-tag 'wasm-block)))
    (call-with-prompt tag
      (lambda ()
        (proc tag))
      ;; The 'return_call' family of instructions need to pass along a
      ;; continuation that can be tail called after aborting to the
      ;; prompt.
      (lambda (_k thunk)
        (handler thunk)))))

;; Debugging/instrumentation hook, called before each instruction.
(define current-instruction-listener
  (make-parameter (lambda (instr path instance stack blocks locals) #t)))

;; Per Andy: Don't bother with br_on_cast right now!
(define (execute instr path instance stack blocks locals)
  (define (runtime-error msg . irritants)
    (let ((path* (reverse path)))
      (raise-exception
       (make-exception
        (make-wasm-runtime-error instr path* instance stack blocks locals)
        (make-exception-with-message
         (format #f "WASM runtime error: ~a" msg))
        (make-exception-with-irritants irritants))
       #:continuable? #t)))
  ;; Stack shorthands.
  (define (push x) (stack-push! stack x))
  (define (push-all lst) (stack-push-all! stack lst))
  (define (pop) (or (stack-pop! stack) (runtime-error "empty stack")))
  (define (pop-n n) (stack-pop-n! stack n))
  (define (peek) (or (stack-peek stack) (runtime-error "empty stack")))
  ;; Macro for let-binding values popped off the stack.
  (define-syntax lets
    (lambda (x)
      (syntax-case x ()
        ((_ (var ...) body ...)
         ;; Popping the stack gets the last arg, so reverse binding
         ;; order.
         (with-syntax (((var ...) (reverse #'(var ...))))
           #'(let* ((var (pop)) ...) body ...))))))
  ;; Control flow helpers.
  (define (call* func)
    (match func
      (($ <wasm-func> proc ($ <func-sig> params))
       (apply proc (pop-n (length params))))
      (x (runtime-error "not a function" x))))
  (define (call func)
    (call-with-values (lambda () (call* func))
      (lambda vals (push-all vals))))
  (define (return* thunk)
    ;; The current function's tag is at the bottom.
    (match blocks
      ((_ ... tag)
       (abort-to-prompt tag thunk))))
  (define (return) (return* #f))
  (define (return-call f) (return* (lambda () (call* f))))
  (define (branch l) (abort-to-prompt (list-ref blocks l) #f))
  (define (block path body branch)
    (call-with-block
     (lambda (block)
       (execute* body path instance stack (cons block blocks) locals))
     (lambda (_) (branch))))
  (define (end) 'end)
  ;; Convenience macros.
  (define-syntax-rule (unop proc)
    (push (proc (pop))))
  (define-syntax-rule (u32-unop proc)
    (unop (lambda (a) (proc (s32->u32 a)))))
  (define-syntax-rule (u64-unop proc)
    (unop (lambda (a) (proc (s64->u64 a)))))
  (define-syntax-rule (binop proc)
    (lets (a b) (push (proc a b))))
  (define-syntax-rule (compare pred)
    (binop (lambda (a b) (if (pred a b) 1 0))))
  (define-syntax-rule (compare1 pred)
    (unop (lambda (a) (if (pred a) 1 0))))
  (define-syntax-rule (s32-binop proc)
    (binop (lambda (a b) (s32-overflow (proc a b)))))
  (define-syntax-rule (s64-binop proc)
    (binop (lambda (a b) (s64-overflow (proc a b)))))
  (define-syntax-rule (u32-binop proc)
    (s32-binop (lambda (a b) (s32-overflow (proc (s32->u32 a) (s32->u32 b))))))
  (define-syntax-rule (u64-binop proc)
    (s64-binop (lambda (a b) (s64-overflow (proc (s64->u64 a) (s64->u64 b))))))
  (define-syntax-rule (u32-compare pred)
    (compare (lambda (a b) (pred (s32->u32 a) (s32->u32 b)))))
  (define-syntax-rule (u64-compare pred)
    (compare (lambda (a b) (pred (s64->u64 a) (s64->u64 b)))))
  ;; Math/bitwise op helpers.
  (define (!= a b) (not (= a b)))
  (define (shl n m k) (modulo (ash n (modulo m k)) (ash 1 k)))
  (define (shl32 n m) (shl n m 32))
  (define (shl64 n m) (shl n m 64))
  (define (shr n m k) (ash n (- (modulo m k))))
  (define (shr32 n m) (shr n m 32))
  (define (shr64 n m) (shr n m 64))
  (define (rotl n m k) (logior (ash n m) (ash n (- (- k m)))))
  (define (rotl32 n m) (rotl n m 32))
  (define (rotl64 n m) (rotl n m 64))
  (define (rotr n m k) (logior (ash n (- m)) (ash n (- k m))))
  (define (rotr32 n m) (rotr n m 32))
  (define (rotr64 n m) (rotr n m 64))
  (define (clz n k)
    (let loop ((i (- k 1))
               (result 0))
      (if (or (= i -1) (logbit? i n))
          result
          (loop (- i 1) (+ result 1)))))
  (define (clz32 n) (clz n 32))
  (define (clz64 n) (clz n 64))
  (define (ctz n k)
    (let loop ((i 0)
               (result 0))
      (if (or (= i k) (logbit? i n))
          result
          (loop (+ i 1) (+ result 1)))))
  (define (ctz32 n) (ctz n 32))
  (define (ctz64 n) (ctz n 64))
  (define (popcnt n k)
    (let loop ((i 0)
               (result 0))
      (cond
       ((= i k) result)
       ((logbit? i n) (loop (+ i 1) (+ result 1)))
       (else (loop (+ i 1) result)))))
  (define (popcnt32 n) (popcnt n 32))
  (define (popcnt64 n) (popcnt n 64))
  (define (wrap n k)
    (remainder n (ash 1 k)))
  (define (wrap8 n) (wrap n 8))
  (define (wrap16 n) (wrap n 16))
  (define (wrap32 n) (wrap n 32))
  (define (wrap64 n) (wrap n 64))
  (define (s32->s64 x) x)
  (define (u32->s64 x) (s32->u32 x))
  (define (copy-sign a b)
    (* (abs a) (/ b (abs b))))
  (define (float->int x pred)
    (let ((y (inexact->exact (truncate x))))
      (if (pred y) y (runtime-error "integer overflow"))))
  (define (float->s32 x) (float->int x s32?))
  (define (float->u32 x) (float->int x u32?))
  (define (float->s64 x) (float->int x s64?))
  (define (float->u64 x) (float->int x u64?))
  (define (reinterpret x make-bv ref)
    (ref (make-bv x) 0))
  (define (reinterpret/s32->f32 x)
    (reinterpret x s32vector f32vector-ref))
  (define (reinterpret/s64->f64 x)
    (reinterpret x s64vector f64vector-ref))
  (define (reinterpret/f32->s32 x)
    (reinterpret x f32vector s32vector-ref))
  (define (reinterpret/f64->s64 x)
    (reinterpret x f64vector s64vector-ref))
  ;; Memory helpers
  (define (memory-ref id)
    (vector-ref (wasm-instance-memories instance) id))
  (define (memory-bytes id)
    (wasm-memory-bytes (memory-ref id)))
  (define (load* id offset ref)
    (let* ((i (+ (s32->u32 (pop)) offset))
           (bv (memory-bytes id)))
      (push (ref bv i))))
  (define (load id offset ref)
    (load* id offset (lambda (bv i) (ref bv i (endianness little)))))
  (define (load-s64 id offset) (load id offset bytevector-s64-ref))
  (define (load-s32 id offset) (load id offset bytevector-s32-ref))
  (define (load-u32 id offset) (load id offset bytevector-u32-ref))
  (define (load-s16 id offset) (load id offset bytevector-s16-ref))
  (define (load-u16 id offset) (load id offset bytevector-u16-ref))
  (define (load-s8 id offset) (load* id offset bytevector-s8-ref))
  (define (load-u8 id offset) (load* id offset bytevector-u8-ref))
  (define (load-f32 id offset) (load id offset bytevector-ieee-single-ref))
  (define (load-f64 id offset) (load id offset bytevector-ieee-double-ref))
  (define (storei* id offset set wrap s->u)
    (let* ((c (wrap (s->u (pop))))
           (i (+ (s32->u32 (pop)) offset))
           (bv (memory-bytes id)))
      (set bv i c)))
  (define (storei id offset set wrap s->u)
    (storei* id offset
             (lambda (bv i c) (set bv i c (endianness little)))
             wrap s->u))
  (define (store-u64 id offset s->u)
    (storei id offset bytevector-u64-set! wrap64 s->u))
  (define (store-u32 id offset s->u)
    (storei id offset bytevector-u32-set! wrap32 s->u))
  (define (store-u16 id offset s->u)
    (storei id offset bytevector-u16-set! wrap16 s->u))
  (define (store-u8 id offset s->u)
    (storei* id offset bytevector-u8-set! wrap8 s->u))
  (define (storef id offset set)
    (let* ((c (pop))
           (i (+ (s32->u32 (pop)) offset))
           (bv (memory-bytes id)))
      (set bv i c (endianness little))))
  (define (store-f32 id offset) (storef id offset bytevector-ieee-single-set!))
  (define (store-f64 id offset) (storef id offset bytevector-ieee-double-set!))
  ;; Reference helpers:
  (define (type-ref type-or-idx)
    (match type-or-idx
      ((? integer? idx)
       (type-ref (vector-ref (wasm-instance-types instance) idx)))
      ((? symbol? sym) sym)
      ;;(($ <sub-type> _ _ type) type)
      (type type)))
  (define (table-ref idx)
    (vector-ref (wasm-instance-tables instance) idx))
  (define (elem-ref idx)
    (vector-ref (wasm-instance-elems instance) idx))
  (define (data-ref idx)
    (vector-ref (wasm-instance-datas instance) idx))
  (define (func-ref idx)
    (vector-ref (wasm-instance-funcs instance) idx))
  (define (string-ref idx)
    (vector-ref (wasm-instance-strings instance) idx))
  (define (can-downcast? x rt)
    (match rt
      (($ <ref-type> nullable? ht)
       (if (wasm-null? x) nullable? (is-a? x (type-ref ht))))))
  ;; Call instrumentation hook then execute the instruction.
  ((current-instruction-listener) path instr instance stack blocks locals)
  (match instr
    ;; Control:
    (('nop) 'nop)
    (('unreachable)
     (runtime-error "unreachable"))
    (('block _ _ body)
     ;; Branching to a 'block' label exits the block.
     (block path body end))
    (('if _ _ consequent alternate)
     ;; Same behavior as branching to 'block', which is to exit.
     (let ((test (= (pop) 0)))
       (block (if test (cons 0 path) (cons 1 path))
              (if test alternate consequent)
              end)))
    (('loop _ _ body)
     (define (iterate)
       ;; Branching to a 'loop' label re-enters the loop.
       (block path body iterate))
     (iterate))
    (('call idx) (call (func-ref idx)))
    (('call_indirect idx _)
     (call (wasm-table-ref (table-ref idx) (pop))))
    (('call_ref _)
     (lets (f)
           (if (wasm-null? f)
               (runtime-error "null function reference" f)
               (call f))))
    (('return) (return))
    (('return_call idx)
     (return-call (func-ref idx)))
    (('return_call_indirect idx _)
     (return-call (wasm-table-ref (table-ref idx) (pop))))
    (('return_call_ref _)
     (lets (f)
           (if (wasm-null? f)
               (runtime-error "null function reference" f)
               (return-call f))))
    (('br l) (branch l))
    (('br_if l) (unless (= (pop) 0) (branch l)))
    (('br_table l* l)
     (lets (n) (branch (if (< n (length l*)) (list-ref l* n) l))))
    ;; Parametric:
    (('drop) (pop))
    (('select)
     (if (= (pop) 0)
         (let ((x (pop)))
           (pop)
           (push x))
         (pop)))
    ;; Locals:
    (('local.get idx) (push (vector-ref locals idx)))
    (('local.set idx) (vector-set! locals idx (pop)))
    (('local.tee idx) (vector-set! locals idx (peek)))
    ;; Globals:
    (('global.get idx)
     (push (wasm-global-ref (wasm-instance-global-ref instance idx))))
    (('global.set idx)
     (wasm-global-set! (wasm-instance-global-ref instance idx) (pop)))
    ;; Numeric:
    (('i32.const x) (push x))
    (('i32.eqz) (compare1 zero?))
    (('i32.eq) (compare =))
    (('i32.ne) (compare !=))
    (('i32.lt_s) (compare <))
    (('i32.lt_u) (u32-compare <))
    (('i32.le_s) (compare <=))
    (('i32.le_u) (u32-compare <=))
    (('i32.gt_s) (compare >))
    (('i32.gt_u) (u32-compare >))
    (('i32.ge_s) (compare >=))
    (('i32.ge_u) (u32-compare >=))
    (('i32.add) (s32-binop +))
    (('i32.sub) (s32-binop -))
    (('i32.mul) (s32-binop *))
    (('i32.div_s) (s32-binop quotient))
    (('i32.div_u) (u32-binop quotient))
    (('i32.rem_s) (s32-binop remainder))
    (('i32.rem_u) (u32-binop remainder))
    (('i32.and) (binop logand))
    (('i32.or) (binop logior))
    (('i32.xor) (binop logxor))
    (('i32.shl) (s32-binop shl32))
    (('i32.shr_s) (s32-binop shr32))
    (('i32.shr_u) (u32-binop shr32))
    (('i32.rotl) (u32-binop rotl32))
    (('i32.rotr) (u32-binop rotr32))
    (('i32.clz) (u32-unop clz32))
    (('i32.ctz) (u32-unop ctz32))
    (('i32.popcnt) (u32-unop popcnt32))
    (('i32.wrap_i64) (unop wrap32))
    (('i32.trunc_f32_s) (unop float->s32))
    (('i32.trunc_f32_u) (unop float->u32))
    (('i32.trunc_f64_s) (unop float->s32))
    (('i32.trunc_f64_u) (unop float->u32))
    (('i32.reinterpret_f32) (unop reinterpret/f32->s32))
    (('i64.const x) (push x))
    (('i64.eqz) (compare1 zero?))
    (('i64.eq) (compare =))
    (('i64.ne) (compare !=))
    (('i64.lt_s) (compare <))
    (('i64.lt_u) (u64-compare <))
    (('i64.le_s) (compare <=))
    (('i64.le_u) (u64-compare <=))
    (('i64.gt_s) (compare >))
    (('i64.gt_u) (u64-compare >))
    (('i64.ge_s) (compare >=))
    (('i64.ge_u) (u64-compare >=))
    (('i64.add) (s64-binop +))
    (('i64.sub) (s64-binop -))
    (('i64.mul) (s64-binop *))
    (('i64.div_s) (s64-binop quotient))
    (('i64.div_u) (u64-binop quotient))
    (('i64.rem_s) (s64-binop remainder))
    (('i64.rem_u) (u64-binop remainder))
    (('i64.and) (binop logand))
    (('i64.or) (binop logior))
    (('i64.xor) (binop logxor))
    (('i64.shl) (s64-binop shl64))
    (('i64.shr_s) (s64-binop shr64))
    (('i64.shr_u) (u64-binop shr64))
    (('i64.rotl) (u64-binop rotl64))
    (('i64.rotr) (u64-binop rotr64))
    (('i64.clz) (u64-unop clz64))
    (('i64.ctz) (u64-unop ctz64))
    (('i64.popcnt) (u64-unop popcnt64))
    (('i64.extend_i32_s) (unop s32->s64))
    (('i64.extend_i32_u) (unop u32->s64))
    (('i64.trunc_f32_s) (unop float->s64))
    (('i64.trunc_f32_u) (unop float->u64))
    (('i64.trunc_f64_s) (unop float->s64))
    (('i64.trunc_f64_u) (unop float->u64))
    (('i64.reinterpret_f64) (unop reinterpret/f64->s64))
    (('f32.const x) (push x))
    (('f32.eq) (compare =))
    (('f32.ne) (compare !=))
    (('f32.lt) (compare <))
    (('f32.le) (compare <=))
    (('f32.gt) (compare >))
    (('f32.ge) (compare >=))
    (('f32.add) (binop +))
    (('f32.sub) (binop -))
    (('f32.mul) (binop *))
    (('f32.div) (binop /))
    (('f32.abs) (unop abs))
    (('f32.neg) (unop -))
    (('f32.ceil) (unop ceiling))
    (('f32.floor) (unop floor))
    (('f32.trunc) (unop truncate))
    (('f32.nearest) (unop round))
    (('f32.sqrt) (unop sqrt))
    (('f32.min) (binop min))
    (('f32.max) (binop max))
    (('f32.copysign) (binop copy-sign))
    (('f32.convert_i32_s) (unop exact->inexact))
    (('f32.convert_i32_u) (u32-unop exact->inexact))
    (('f32.convert_i64_s) (unop exact->inexact))
    (('f32.convert_i64_u) (u64-unop exact->inexact))
    (('f32.demote_f64) #t)
    (('f32.reinterpret_i32) (unop reinterpret/s32->f32))
    (('f64.const x) (push x))
    (('f64.eq) (compare =))
    (('f64.ne) (compare !=))
    (('f64.lt) (compare <))
    (('f64.le) (compare <=))
    (('f64.gt) (compare >))
    (('f64.ge) (compare >=))
    (('f64.add) (binop +))
    (('f64.sub) (binop -))
    (('f64.mul) (binop *))
    (('f64.div) (binop /))
    (('f64.abs) (unop abs))
    (('f64.neg) (unop -))
    (('f64.ceil) (unop ceiling))
    (('f64.floor) (unop floor))
    (('f64.trunc) (unop truncate))
    (('f64.nearest) (unop round))
    (('f64.sqrt) (unop sqrt))
    (('f64.min) (binop min))
    (('f64.max) (binop max))
    (('f64.copysign) (binop copy-sign))
    (('f64.convert_i32_s) (unop exact->inexact))
    (('f64.convert_i32_u) (u32-unop exact->inexact))
    (('f64.convert_i64_s) (unop exact->inexact))
    (('f64.convert_i64_u) (u64-unop exact->inexact))
    (('f64.promote_f32) #t)
    (('f64.reinterpret_i64) (unop reinterpret/s64->f64))
    ;; Linear memory:
    (('i32.load ($ <mem-arg> id offset _)) (load-s32 id offset))
    (('i32.load16_s ($ <mem-arg> id offset _)) (load-s16 id offset))
    (('i32.load16_u ($ <mem-arg> id offset _)) (load-u16 id offset))
    (('i32.load8_s ($ <mem-arg> id offset _)) (load-s8 id offset))
    (('i32.load8_u ($ <mem-arg> id offset _)) (load-u8 id offset))
    (('i32.store ($ <mem-arg> id offset _)) (store-u32 id offset s32->u32))
    (('i32.store16 ($ <mem-arg> id offset _)) (store-u16 id offset s32->u32))
    (('i32.store8 ($ <mem-arg> id offset _)) (store-u8 id offset s32->u32))
    (('i64.load ($ <mem-arg> id offset _)) (load-s64 id offset))
    (('i64.load32_s ($ <mem-arg> id offset _)) (load-s32 id offset))
    (('i64.load32_u ($ <mem-arg> id offset _)) (load-u32 id offset))
    (('i64.load16_s ($ <mem-arg> id offset _)) (load-s16 id offset))
    (('i64.load16_u ($ <mem-arg> id offset _)) (load-u16 id offset))
    (('i64.load8_s ($ <mem-arg> id offset _)) (load-s8 id offset))
    (('i64.load8_u ($ <mem-arg> id offset _)) (load-u8 id offset))
    (('f32.load ($ <mem-arg> id offset _)) (load-f32 id offset))
    (('f64.load ($ <mem-arg> id offset _)) (load-f64 id offset))
    (('i64.store ($ <mem-arg> id offset _)) (store-u64 id offset s64->u64))
    (('i64.store32 ($ <mem-arg> id offset _)) (store-u32 id offset s64->u64))
    (('i64.store16 ($ <mem-arg> id offset _)) (store-u16 id offset s64->u64))
    (('i64.store8 ($ <mem-arg> id offset _)) (store-u8 id offset s64->u64))
    (('f32.store ($ <mem-arg> id offset _)) (store-f32 id offset))
    (('f64.store ($ <mem-arg> id offset _)) (store-f64 id offset))
    (('memory.size id) (push (wasm-memory-size (memory-ref id))))
    (('memory.grow id) (push (wasm-memory-grow! (memory-ref id) (pop))))
    ;; Reference types:
    (('table.get idx) (push (wasm-table-ref (table-ref idx) (pop))))
    (('table.set idx)
     (lets (i val) (wasm-table-set! (table-ref idx) i val)))
    (('table.size idx) (push (wasm-table-size (table-ref idx))))
    (('table.grow idx)
     (lets (x n) (push (wasm-table-grow! (table-ref idx) n x))))
    (('table.init dst src)
     (lets (d s n) (wasm-table-init! (table-ref dst) d (elem-ref src) s n)))
    (('table.fill idx)
     (lets (i val n) (wasm-table-fill! (table-ref idx) i val n)))
    (('table.copy dst src)
     (lets (d s n) (wasm-table-copy! (table-ref dst) d (table-ref src) s n)))
    (('elem.drop idx) (wasm-instance-drop-elem! instance idx))
    (('ref.eq) (compare eq?))
    (('ref.null t) (push (make-wasm-null (type-ref t))))
    (('ref.as_non_null)
     (let ((x (peek)))
       (when (wasm-null? x)
         (runtime-error "null value" x))))
    (('ref.is_null) (compare1 wasm-null?))
    (('ref.func idx) (push (func-ref idx)))
    (('ref.test rt)
     (push (if (can-downcast? (pop) rt) 1 0)))
    (('ref.cast rt)
     (let ((x (peek)))
       (unless (can-downcast? x rt)
         (runtime-error "invalid cast" x))))
    (('br_on_cast l rt1 rt2)
     (when (can-downcast? (peek) rt2)
       (branch l)))
    (('br_on_cast_fail l rt1 rt2)
     (unless (can-downcast? (peek) rt2)
       (branch l)))
    (('ref.i31)
     (let ((x (peek)))
       (unless (s31? (peek))
         (runtime-error "invalid i31" x))))
    (('i31.get_s) #t)
    (('i31.get_u) (unop s31->u31))
    (('struct.new t)
     (let ((type (type-ref t)))
       (push
        (make-wasm-struct type
                          (reverse
                           (map (lambda (_) (pop))
                                (struct-type-fields (resolve-type type))))))))
    (('struct.new_default t)
     (push
      (let ((type (type-ref t)))
        (make-wasm-struct type
                          (map (lambda (field)
                                 (default-for-type (field-type field)))
                               (struct-type-fields (resolve-type type)))))))
    (('struct.get _ field) (push (wasm-struct-ref (pop) field)))
    (('struct.get_s _ field) (push (wasm-struct-ref-signed (pop) field)))
    (('struct.get_u _ field) (push (wasm-struct-ref-unsigned (pop) field)))
    (('struct.set _ field) (lets (s x) (wasm-struct-set! s field x)))
    (('array.new t)
     (lets (fill k) (push (make-wasm-array (type-ref t) (s32->u32 k) fill))))
    (('array.new_fixed t k)
     (let ((array (make-wasm-array (type-ref t) k)))
       (do ((i (- k 1) (- i 1)))
           ((< i 0))
         (wasm-array-set! array i (pop)))
       (push array)))
    (('array.new_default t)
     (lets (k)
           (let ((type (type-ref t)))
             (push (make-wasm-array type k
                                    (default-for-type
                                      (array-type-type
                                       (resolve-type type))))))))
    (('array.new_data t d)
     (lets (offset k)
           (let ((array (make-wasm-array (type-ref t) k)))
             (wasm-array-init-data! array 0 (data-ref d) offset k)
             (push array))))
    (('array.new_elem t d)
     (lets (offset k)
           (let ((array (make-wasm-array (type-ref t) k)))
             (wasm-array-init-elem! array 0 (elem-ref d) offset k)
             (push array))))
    (('array.init_data t d)
     (lets (a at offset length)
           (wasm-array-init-data! a at (data-ref d) offset length)))
    (('array.init_elem t d)
     (lets (a at offset length)
           (wasm-array-init-elem! a at (elem-ref d) offset length)))
    (('array.len) (lets (a) (push (wasm-array-length a))))
    (('array.get _) (lets (a i) (push (wasm-array-ref a (s32->u32 i)))))
    (('array.get_s _) (lets (a i) (push (wasm-array-ref-signed a (s32->u32 i)))))
    (('array.get_u _) (lets (a i) (push (wasm-array-ref-unsigned a (s32->u32 i)))))
    (('array.set _) (lets (a i x) (wasm-array-set! a (s32->u32 i) x)))
    (('array.fill _) (lets (a d x n) (wasm-array-fill! a d x n)))
    (('array.copy _ _) (lets (dst d src s n) (wasm-array-copy! dst d src s n)))
    (('extern.internalize _) #t)
    (('extern.externalize _) #t)
    ;; Strings:
    (('string.const idx) (push (string-ref idx)))
    (('string.new_lossy_utf8_array)
     (lets (array start end) (push (wasm-array->string array start end))))
    (('string.encode_wtf8_array)
     (lets (str array start)
           (wasm-array-encode-string! array str start)
           (push (string-utf8-length str))))
    (((or 'string.measure_utf8 'string.measure_wtf8))
     (lets (str) (push (string-utf8-length str))))
    (('string.as_iter)
     (lets (str)
           (push (make-wasm-string-iterator str 0))))
    (('stringview_iter.next)
     (lets (iter) (push (wasm-string-iterator-next! iter))))
    (('stringview_iter.advance)
     (lets (iter k) (push (wasm-string-iterator-advance! iter k))))
    (_ (runtime-error "unimplemented" instr))))

(define (execute* instrs path instance stack blocks locals)
  (let loop ((instrs instrs) (i 0))
    (match instrs
      (() 'end)
      ((instr . rest)
       (execute instr (cons i path) instance stack blocks locals)
       (loop rest (+ i 1))))))
