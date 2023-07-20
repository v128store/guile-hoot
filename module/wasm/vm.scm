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
  #:use-module (wasm parse)
  #:use-module (wasm types)
  #:export (make-wasm-module
            wasm-module?
            wasm-module-export-ref

            make-wasm-global
            wasm-global?
            wasm-global-ref
            wasm-global-set!
            wasm-global-mutable?

            wasm-position?
            wasm-position-instructions
            wasm-position-index

            wasm-stack?
            wasm-stack-items

            make-wasm-instance
            wasm-instance?
            wasm-instance-module
            wasm-instance-export-ref

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

(define (is-a? x type)
  (case type
    ((i32) (s32? x))
    (else #f)))


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
     (map (match-lambda
            (($ <func> _ ($ <type-use> _ ($ <type> _ sig))) sig))
          (wasm-funcs wasm))))
  (define (validation-error msg . irritants)
    (raise-exception
     (make-exception
      (make-wasm-validation-error wasm)
      (make-exception-with-message
       (format #f "WASM validation error: ~a" msg))
      (make-exception-with-irritants irritants))))
  (define (push stack . types)
    (let loop ((stack stack)
               (types types))
      (match types
        (() stack)
        ((head . tail)
         (loop (cons head stack) tail)))))
  (define (pop stack . types)
    (let loop ((stack stack)
               (types types))
      (match types
        (() stack)
        ((type . rest-types)
         (match stack
           ((stack-type . rest-stack)
            (if (eq? stack-type type)
                (loop rest-stack rest-types)
                (validation-error
                 (format #f "stack type mismatch; expected ~a, got ~a"
                         type stack-type))))
           (_
            (validation-error "not enough values on stack")))))))
  (define (assert-s32 x)
    (unless (s32? x)
      (validation-error "i32 constant out of range" x)))
  (define (validate-global global)
    ;; There's redundancy here with the more general function
    ;; validator, but there are only a very limited set of constant
    ;; instructions that are valid for initializing globals.
    (define (validate-instr instr stack)
      ;; TODO: Support all constant instructions.
      (match instr
        (('i32.const x)
         (assert-s32 x)
         (push stack 'i32))
        (invalid
         (validation-error "invalid global initializer instruction"
                           global instr))))
    (match global
      (($ <global> _ type instrs)
       (let loop ((instrs instrs)
                  (stack '()))
         (match instrs
           (() #t)
           ((instr . rest)
            (loop rest (validate-instr instr stack))))))))
  (define (validate-func func)
    (match func
      (($ <func> _ ($ <type-use> _ ($ <type> _ type)) _ body)
       (define local-types
         (list->vector
          (append (match type
                    (($ <func-sig> (($ <param> _ types) ...))
                     types))
                  (map local-type (func-locals func)))))
       (define (lookup-block-type bt)
         (match bt
           (#f (make-func-sig '() '()))
           ((? exact-integer? idx) (type-val (list-ref (wasm-types wasm) idx)))
           ((? symbol? type) (make-func-sig '() (list type)))))
       (define (block-type-params bt)
         (match bt
           (($ <func-sig> ((= param-type param-types) ...) _)
            param-types)))
       (define block-type-results func-sig-results)
       (define (check-types stack types)
         (let ((stack-types (reverse stack)))
           (unless (equal? types stack-types)
             (validation-error
              (format #f "type mismatch; expected ~a, got ~a"
                      types stack-types)))))
       (define (check-bounds v idx)
         (unless (and (>= idx 0) (< idx (vector-length v)))
           (validation-error "index out of bounds" idx)))
       (define (validate-instr instr stack control)
         (define (check-label l)
           (unless (and (>= l 0) (< l (length control)))
             (validation-error "invalid label" l)))
         (match instr
           ;; Control
           (('nop) stack)
           (('unreachable) #f)
           (('if _ (= lookup-block-type block-type) consequent alternate)
            (let ((new-stack (pop stack 'i32)))
              (check-types new-stack (block-type-params block-type))
              (validate-block block-type consequent new-stack control)
              (validate-block block-type alternate new-stack control)))
           (((or 'block 'loop) _ (= lookup-block-type block-type) body)
            (check-types stack (block-type-params block-type))
            (validate-block block-type body stack control))
           (('call idx)
            (match (vector-ref func-sigs idx)
              (($ <func-sig> (($ <param> _ param-types) ...)
                             (result-types ...))
               (apply push (apply pop stack param-types) result-types))))
           (('return)
            (match control
              ((_ ... block)
               (check-types stack (block-type-results block))
               #f)))
           (('br l)
            (check-label l)
            (check-types stack (block-type-results (list-ref control l)))
            #f)
           (('br_if l)
            (let ((new-stack (pop stack 'i32)))
              (check-label l)
              (check-types new-stack (block-type-results (list-ref control l)))
              new-stack))
           ;; Locals
           (('local.get idx)
            (push stack (vector-ref local-types idx)))
           (('local.set idx)
            (pop stack (vector-ref local-types idx)))
           (('local.tee idx)
            (let ((type (vector-ref local-types idx)))
              (push (pop stack type) type)))
           ;; Globals
           (('global.get idx)
            (match (vector-ref global-types idx)
              (($ <global-type> _ type)
                 (push stack type))))
           (('global.set idx)
            (match (vector-ref global-types idx)
              (($ <global-type> mutable? type)
               (unless mutable?
                 (validation-error "global is immutable" idx))
               (pop stack type))))
           ;; Numeric
           (('i32.const x)
            (assert-s32 x)
            (push stack 'i32))
           (((or 'i32.eqz))
            (push (pop stack 'i32) 'i32))
           (((or 'i32.add 'i32.sub 'i32.mul 'i32.div_s 'i32.div_u
                 'i32.rem_s 'i32.rem_u 'i32.eq 'i32.ne 'i32.lt_s 'i32.lt_u
                 'i32.le_s 'i32.le_u 'i32.gt_s 'i32.gt_u 'i32.ge_s 'i32.ge_u
                 'i32.and 'i32.or 'i32.xor 'i32.shl 'i32.shr_s 'i32.shr_u
                 'i32.rotl 'i32.rotr))
            (push (pop stack 'i32 'i32) 'i32))
           (instr
            (validation-error "unimplemented instruction" instr))))
       (define (validate-block block-type instrs stack control)
         (let ((control* (cons block-type control)))
           (let loop ((instrs instrs)
                      (stack stack))
             (and stack
                  (match instrs
                    (()
                     (check-types stack (block-type-results block-type))
                     stack)
                    ((instr . rest)
                     (loop rest (validate-instr instr stack control*))))))))
       (validate-block type body '() '()))))
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

(define-record-type <wasm-instance>
  (%make-wasm-instance module globals funcs exports)
  wasm-instance?
  (module wasm-instance-module)
  (globals wasm-instance-globals)
  (funcs wasm-instance-funcs)
  (exports wasm-instance-exports))

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
;; TODO: Support memories.
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
            (global-vec (make-vector (+ n-global-imports (length globals))))
            (func-vec (make-vector (+ n-func-imports (length funcs))))
            (export-table (make-hash-table))
            (instance (%make-wasm-instance module global-vec func-vec
                                           export-table)))
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
                  (apply values (reverse (wasm-stack-items stack)))))
              (make-wasm-func wasm-proc sig)))))
       ;; Process imports.
       (let loop ((wasm-imports wasm-imports)
                  (global-idx 0)
                  (func-idx 0))
         (match wasm-imports
           (() #t)
           ((($ <import> mod name 'func _ ($ <type-use> _ ($ <type> _ sig))) . rest)
            (match (lookup-import mod name)
              ((? procedure? proc)
               (vector-set! func-vec func-idx (make-wasm-func proc sig))
               (loop rest global-idx (+ func-idx 1)))))
           ((($ <import> mod name 'global _ type) . rest)
            (match (lookup-import mod name)
              ((? wasm-global? global)
               (vector-set! global-vec global-idx global)
               (loop rest (+ global-idx 1) func-idx))))))
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
  (define-syntax-rule (binop proc)
    (let ((b (pop)) (a (pop))) (push (proc a b))))
  (define-syntax-rule (compare pred)
    (binop (lambda (a b) (if (pred a b) 1 0))))
  (define-syntax-rule (s32-binop proc)
    (binop (lambda (a b) (s32-overflow (proc a b)))))
  (define-syntax-rule (u32-binop proc)
    (s32-binop (lambda (a b) (s32-overflow (proc (s32->u32 a) (s32->u32 b))))))
  (define-syntax-rule (u32-compare pred)
    (compare (lambda (a b) (pred (s32->u32 a) (s32->u32 b)))))
  ;; Math/bitwise op helpers.
  (define (!= a b) (not (= a b)))
  (define (eqz a) (if (= a 0) 1 0))
  (define shl ash)
  (define (shr n k) (ash n (- k)))
  (define (rotl32 n k) (logior (shl n k) (shr n (- 32 k))))
  (define (rotr32 n k) (logior (shr n k) (shl n (- 32 k))))
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
        (call-with-values (lambda ()
                            (apply proc (map (lambda (_) (pop)) params)))
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