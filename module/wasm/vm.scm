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
  #:export (make-wasm-module
            wasm-module?
            wasm-module-export-ref

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

            wasm-position?
            wasm-position-instructions
            wasm-position-index

            wasm-stack?
            wasm-stack-items

            make-wasm-instance
            wasm-instance?
            wasm-instance-module
            wasm-instance-export-ref
            wasm-instance-export-names

            current-instruction-listener

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

(define (s32->u32 x)
  (logand x #xffffFFFF))

(define (s32-overflow x)
  (centered-remainder x (ash 1 32)))

(define (s64? x)
  (and (exact-integer? x) (< (- (ash -1 63) 1) x (ash 1 63))))

(define (s64->u64 x)
  (logand x #xffffFFFFffffFFFF))

(define (s64-overflow x)
  (centered-remainder x (ash 1 64)))

(define (f32? x)
  (or (inexact? x) (exact-integer? x)))

(define (f64? x)
  (or (inexact? x) (exact-integer? x)))

(define (is-a? x type)
  (match type
    ('i32 (s32? x))
    ('i64 (s64? x))
    ('f32 (f32? x))
    ('f64 (f64? x))
    (_ #f)))


;;;
;;; Modules
;;;

(define-record-type <wasm-module>
  (%make-wasm-module wasm)
  wasm-module?
  (wasm wasm-module-wasm))

(define-exception-type &wasm-error &error
  make-wasm-error
  wasm-error?)

(define-exception-type &wasm-validation-error &wasm-error
  make-wasm-validation-error
  wasm-validation-error?
  (wasm wasm-validation-error-wasm))

;; TODO: Trace instruction position within function to give context to
;; validation errors.
(define (make-wasm-module* wasm)
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
                (($ <import> _ _ 'func _ ($ <type-use> _ ($ <type> _ sig)))
                 sig)
                (_ #f))
              (wasm-imports wasm))
             (map (match-lambda
                    (($ <func> _ ($ <type-use> _ ($ <type> _ sig))) sig))
                  (wasm-funcs wasm)))))
  (define memories
    (list->vector
     (append (filter-map (match-lambda
                           (($ <import> _ _ 'memory _ type) type)
                           (_ #f))
                         (wasm-imports wasm))
             (wasm-memories wasm))))
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
  (define (validate-constant instr)
    (match instr
      (('i32.const x) (assert-s32 x))
      (('i64.const x) (assert-s64 x))
      (('f32.const x) (assert-f32 x))
      (('f64.const x) (assert-f64 x))))
  (define (validate-global global)
    (define (validate-instr ctx instr)
      (match instr
        ;; TODO: Support all constant instructions.
        (((or 'i32.const 'i64.const 'f32.const 'f64.const) . _)
         (match (apply-stack-effect ctx (compute-stack-effect ctx instr))
           (($ <invalid-ctx> reason)
            (validation-error reason instr))
           (ctx ctx)))))
    (match global
      (($ <global> _ _ instrs)
       (let loop ((instrs instrs)
                  (ctx '()))
         (match instrs
           (() #t)
           ((instr . rest)
            (loop rest (validate-instr (initial-ctx wasm global) instr))))))))
  (define (validate-func func)
    (match func
      (($ <func> _ ($ <type-use> _ ($ <type> _ type)) _ body)
       (define (lookup-block-type bt)
         (match bt
           (#f (make-func-sig '() '()))
           ((? exact-integer? idx) (type-val (list-ref (wasm-types wasm) idx)))
           ((? symbol? type) (make-func-sig '() (list type)))))
       (define (push-block* ctx bt loop?)
         (match bt
           (($ <func-sig> (($ <param> _ params) ...) results)
            (push-block ctx #f params results #:is-loop? loop?))))
       (define (check-memory id)
         (unless (< -1 id (vector-length memories))
           (error "invalid memory" id)))
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
              (((or 'i32.const 'i64.const 'f32.const 'f64.const) . _)
               (validate-constant instr))
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
  (%make-wasm-module wasm))

(define (make-wasm-module bin)
  "Create a new WASM module from the WASM binary BIN.  BIN may be a
bytevector, an input port, or a <wasm> record produced by
'resolve-wasm' in the (wasm resolve) module."
  (make-wasm-module*
   (cond
    ((wasm? bin) bin)
    ((bytevector? bin)
     (call-with-input-bytevector bin parse-wasm))
    ((port? bin)
     (parse-wasm bin))
    (else
     (error "not a WASM binary" bin)))))


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

;; Branch + index context for debugging.
(define-record-type <wasm-position>
  (make-wasm-position instructions index)
  wasm-position?
  (instructions wasm-position-instructions)
  (index wasm-position-index set-wasm-position-index!))

(define (wasm-position-increment! pos)
  (set-wasm-position-index! pos (+ (wasm-position-index pos) 1)))

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

(define (clamp-to-limits x limits)
  (match limits
    (($ <limits> min max)
     (let ((max* (or max %max-pages)))
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
  (let ((size* (clamp-to-limits size limits)))
    (%make-wasm-memory (make-bytevector/pages size*) size* limits)))

(define (wasm-memory-grow! memory n)
  (match memory
    (($ <wasm-memory> old-bytes old-size limits)
     (if (= n 0)
         old-size
         (let ((new-size (clamp-to-limits
                          (+ (wasm-memory-size memory) n) limits)))
           (if (= new-size old-size)
               -1
               (let ((new-bytes (make-bytevector/pages new-size)))
                 (bytevector-copy! old-bytes 0 new-bytes 0
                                   (* old-size %page-size))
                 (set-wasm-memory-bytes! memory new-bytes)
                 (set-wasm-memory-size! memory new-size)
                 old-size)))))))

(define-record-type <wasm-instance>
  (%make-wasm-instance module globals funcs memories exports)
  wasm-instance?
  (module wasm-instance-module)
  (globals wasm-instance-globals)
  (funcs wasm-instance-funcs)
  (memories wasm-instance-memories)
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

;; TODO: Support tables.
(define* (make-wasm-instance module #:key (imports '()))
  (define (lookup-import mod name)
    (assoc-ref (or (assoc-ref imports mod) '()) name))
  (match module
    (($ <wasm-module>
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
            (global-vec (make-vector (+ n-global-imports (length globals))))
            (func-vec (make-vector (+ n-func-imports (length funcs))))
            (memory-vec (make-vector (+ n-memory-imports (length memories))))
            (export-table (make-hash-table))
            (instance (%make-wasm-instance module global-vec func-vec
                                           memory-vec export-table)))
       (define (type-check vals types)
         (unless (every is-a? vals types)
           (error (format #f "type mismatch; expected ~a" types)
                  vals)))
       ;; TODO: Handle functions imported from other WASM modules.
       (define (make-import-closure proc sig)
         (let ((result-types (func-sig-results sig)))
           (lambda args
             (call-with-values (lambda () (apply proc args))
               (lambda vals
                 (type-check vals result-types)
                 (apply values vals))))))
       (define (make-export-closure name func)
         (match func
           (($ <wasm-func> proc ($ <func-sig> (($ <param> _ param-types) ...)))
            (define (wrap . args)
              (type-check args param-types)
              (apply proc args))
            (set-procedure-property! wrap 'name (string->symbol name))
            wrap)))
       (define (default-for-type type)
         (match type
           ('i32 0)))
       (define (instantiate-func func)
         (match func
           (($ <func> _ ($ <type-use> _ ($ <type> _ sig)) locals body)
            (let* ((local-types (map local-type locals))
                   (n-params (length (func-sig-params sig)))
                   (n-results (length (func-sig-results sig)))
                   (n-locals (length local-types)))
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
                  ;; Execute instructions in body.
                  (call-with-block
                   (lambda (tag)
                     (execute* body (list func) instance stack (list tag) locals))
                   (lambda () 'return))
                  (apply values (stack-pop-n! stack n-results))))
              (make-wasm-func wasm-proc sig)))))
       ;; Process imports.
       (let loop ((wasm-imports wasm-imports)
                  (global-idx 0)
                  (func-idx 0)
                  (memory-idx 0))
         (match wasm-imports
           (() #t)
           ((($ <import> mod name 'func _ ($ <type-use> _ ($ <type> _ sig))) . rest)
            (match (lookup-import mod name)
              ((? procedure? proc)
               (vector-set! func-vec func-idx (make-wasm-func proc sig))
               (loop rest global-idx (+ func-idx 1) memory-idx))
              (x (instance-error "invalid function import" x))))
           ((($ <import> mod name 'global _ type) . rest)
            (match (lookup-import mod name)
              ((? wasm-global? global)
               (vector-set! global-vec global-idx global)
               (loop rest (+ global-idx 1) func-idx memory-idx))
              (x (instance-error "invalid global import" x))))
           ((($ <import> mod name 'memory _ type) . rest)
            (match (lookup-import mod name)
              ((? wasm-memory? memory)
               (vector-set! memory-vec memory-idx memory)
               (loop rest global-idx func-idx (+ memory-idx 1)))
              (x (instance-error "invalid memory import" x))))))
       ;; Initialize globals.
       (let loop ((globals globals)
                  (idx n-global-imports))
         (match globals
           (() #t)
           (((and ($ <global> _ ($ <global-type> mutable? type) init) global) . rest)
            ;; Invoke the VM to process the constant expressions that
            ;; produce the initial value.
            (let ((stack (make-wasm-stack)))
              (execute* init (list global) instance stack '() #())
              (let ((global (make-wasm-global (stack-pop! stack) mutable?)))
                (vector-set! global-vec idx global))
              (loop rest (+ idx 1))))))
       ;; Initialize functions.
       (let loop ((funcs funcs)
                  (idx n-func-imports))
         (match funcs
           (() #t)
           ((func . rest)
            (vector-set! func-vec idx (instantiate-func func))
            (loop rest (+ idx 1)))))
       ;; Initialize memories.
       (let loop ((memories memories)
                  (idx n-memory-imports))
         (match memories
           (() #t)
           ((($ <mem-type> (and ($ <limits> min) limits)) . rest)
            (vector-set! memory-vec idx (make-wasm-memory min limits))
            (loop rest (+ idx 1)))))
       ;; Copy data into memory.
       (for-each (match-lambda
                   ((and ($ <data> _ mode mem-id instrs init) data)
                    ;; Invoke the VM to process the constant
                    ;; expressions that produce the offset value.
                    (let ((stack (make-wasm-stack))
                          (memory (vector-ref memory-vec mem-id)))
                      (execute* instrs (list data) instance stack '() #())
                      (let ((offset (stack-pop! stack)))
                        (bytevector-copy! init 0 (wasm-memory-bytes memory)
                                          offset (bytevector-length init))))))
                 datas)
       ;; Call start function, if present.
       (when start ((wasm-func-proc (vector-ref func-vec start))))
       ;; Populate export table.
       (for-each (match-lambda
                   (($ <export> name 'func idx)
                    (let ((proc (make-export-closure name (vector-ref func-vec idx))))
                      (hash-set! export-table name proc)))
                   (($ <export> name 'global idx)
                    (hash-set! export-table name (vector-ref global-vec idx))))
                 exports)
       instance))))

(define (wasm-instance-export-ref instance name)
  (hash-ref (wasm-instance-exports instance) name))

(define (wasm-instance-export-names instance)
  (hash-fold (lambda (k v memo) (cons k memo))
             '() (wasm-instance-exports instance)))

(define (wasm-instance-global-ref instance idx)
  (vector-ref (wasm-instance-globals instance) idx))

;; Blocks are delimited by a prompt so that 'br', 'return' and friends
;; can abort to that prompt.
(define (call-with-block proc handler)
  (let ((tag (make-prompt-tag 'wasm-block)))
    (call-with-prompt tag
      (lambda ()
        (proc tag))
      (lambda (_k)
        (handler)))))

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
        (make-exception-with-irritants irritants)))))
  ;; Stack shorthands.
  (define (push x) (stack-push! stack x))
  (define (push-all lst) (stack-push-all! stack lst))
  (define (pop) (or (stack-pop! stack) (runtime-error "empty stack")))
  (define (pop-n n) (stack-pop-n! stack n))
  (define (peek) (or (stack-peek stack) (runtime-error "empty stack")))
  ;; Control helpers.
  (define (block body branch)
    (call-with-block
     (lambda (block)
       (execute* body path instance stack (cons block blocks) locals))
     branch))
  (define (end) 'end)
  ;; Convenience macros.
  (define-syntax-rule (unop proc)
    (push (proc (pop))))
  (define-syntax-rule (u32-unop proc)
    (unop (lambda (a) (proc (s32->u32 a)))))
  (define-syntax-rule (u64-unop proc)
    (unop (lambda (a) (proc (s64->u64 a)))))
  (define-syntax-rule (binop proc)
    (let ((b (pop)) (a (pop))) (push (proc a b))))
  (define-syntax-rule (compare pred)
    (binop (lambda (a b) (if (pred a b) 1 0))))
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
  (define (eqz a) (if (= a 0) 1 0))
  (define shl ash)
  (define (shr n k) (ash n (- k)))
  (define (rotl n m k) (logior (shl n m) (shr n (- k m))))
  (define (rotl32 n m) (rotl n m 32))
  (define (rotl64 n m) (rotl n m 64))
  (define (rotr n m k) (logior (shr n m) (shl n (- k m))))
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
    (modulo n (ash 1 k)))
  (define (wrap8 n) (wrap n 8))
  (define (wrap16 n) (wrap n 16))
  (define (wrap32 n) (wrap n 32))
  (define (wrap64 n) (wrap n 64))
  (define (copy-sign a b)
    (* (abs a) (/ b (abs b))))
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
  ;; Call instrumentation hook then execute the instruction.
  ((current-instruction-listener) path instr instance stack blocks locals)
  (match instr
    ;; Control:
    (('nop) 'nop)
    (('unreachable)
     (runtime-error "unreachable"))
    (('block _ _ body)
     ;; Branching to a 'block' label exits the block.
     (block body end))
    (('if _ _ consequent alternate)
     ;; Same behavior as branching to 'block', which is to exit.
     (block (if (= (pop) 0) alternate consequent) end))
    (('loop _ _ body)
     (define (iterate)
       ;; Branching to a 'loop' label re-enters the loop.
       (block body iterate))
     (iterate))
    (('call idx)
     (match (vector-ref (wasm-instance-funcs instance) idx)
       (($ <wasm-func> proc ($ <func-sig> params))
        ;; Pop n args, apply proc, and push m return values.
        (call-with-values (lambda () (apply proc (pop-n (length params))))
          (lambda vals (push-all vals))))))
    (('return)
     ;; The current function's tag is at the bottom.
     (match blocks
       ((_ ... tag)
        (abort-to-prompt tag))))
    (('br l)
     (abort-to-prompt (list-ref blocks l)))
    (('br_if l)
     (unless (= (pop) 0)
       (abort-to-prompt (list-ref blocks l))))
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
    (('i32.eqz) (unop eqz))
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
    (('i32.shl) (s32-binop shl))
    (('i32.shr_s) (s32-binop shr))
    (('i32.shr_u) (u32-binop shr))
    (('i32.rotl) (u32-binop rotl32))
    (('i32.rotr) (u32-binop rotr32))
    (('i32.clz) (u32-unop clz32))
    (('i32.ctz) (u32-unop ctz32))
    (('i32.popcnt) (u32-unop popcnt32))
    (('i64.const x) (push x))
    (('i64.eqz) (unop eqz))
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
    (('i64.shl) (s64-binop shl))
    (('i64.shr_s) (s64-binop shr))
    (('i64.shr_u) (u64-binop shr))
    (('i64.rotl) (u64-binop rotl64))
    (('i64.rotr) (u64-binop rotr64))
    (('i64.clz) (u64-unop clz64))
    (('i64.ctz) (u64-unop ctz64))
    (('i64.popcnt) (u64-unop popcnt64))
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
    (_ (runtime-error "unimplemented" instr))))

(define (execute* instrs path instance stack blocks locals)
  (let* ((pos (make-wasm-position instrs 0))
         (path* (cons pos path)))
    (let loop ((instrs instrs))
      (match instrs
        (() 'end)
        ((instr . rest)
         (execute instr path* instance stack blocks locals)
         (wasm-position-increment! pos)
         (loop rest))))))
