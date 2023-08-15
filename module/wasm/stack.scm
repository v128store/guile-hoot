;;; WebAssembly assembler
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
;;; Optimizer for WebAssembly.
;;;
;;; Code:

(define-module (wasm stack)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map filter-map))
  #:use-module (srfi srfi-9)
  #:use-module (wasm types)
  #:export (<ctx>
            ctx?
            ctx-func-info
            ctx-block
            ctx-stack

            <unreachable-ctx>
            unreachable-ctx?
            unreachable-ctx-block
            unreachable-ctx-stack

            <invalid-ctx>
            invalid-ctx?
            invalid-ctx-reason

            initial-ctx
            push-block
            compute-stack-effect
            apply-stack-effect
            fallthrough))

(define-record-type <func-info>
  (%make-func-info types funcs globals tables tags locals)
  func-info?
  (types func-info-types)
  (funcs func-info-funcs)
  (globals func-info-globals)
  (tables func-info-tables)
  (tags func-info-tags)
  (locals func-info-locals))

(define-record-type <ctx>
  (make-ctx func-info block stack)
  ctx?
  (func-info ctx-func-info)
  (block ctx-block)
  (stack ctx-stack))

(define-record-type <unreachable-ctx>
  (make-unreachable-ctx func-info block stack)
  unreachable-ctx?
  (func-info unreachable-ctx-func-info)
  (block unreachable-ctx-block)
  (stack unreachable-ctx-stack))

(define-record-type <invalid-ctx>
  (make-invalid-ctx reason)
  invalid-ctx?
  (reason invalid-ctx-reason))

(define-record-type <block>
  (make-block id branch-arg-types result-types parent)
  block?
  (id block-id)
  ;; If you jump to this block's label, what types do you pass?  Usually
  ;; the block results, but for loops it's the loop parameters.
  (branch-arg-types block-branch-arg-types)
  ;; When control falls through the end of a block, what types must be
  ;; on the stack?
  (result-types block-result-types)
  (parent block-parent))

(define-record-type <stack-effect>
  (make-stack-effect params results block-end?)
  stack-effect?
  (params stack-effect-params)
  ;; Results can be #f if the effect causes an exit.
  (results stack-effect-results)
  ;; The stack at the end of a block is expected to contain the param
  ;; types and nothing else below them.
  (block-end? stack-effect-block-end?))

(define (make-func-info wasm func)
  (define types
    (list->vector
     (append-map (match-lambda
                  (($ <rec-group> (($ <type> id type) ...))
                   (map cons id type))
                  (($ <type> id type)
                   (list (cons id type))))
                 (wasm-types wasm))))
  (define (select-imports kind)
    (filter-map (lambda (import)
                  (and (eq? (import-kind import) kind)
                       (cons (import-id import) (import-type import))))
                (wasm-imports wasm)))
  (define funcs
    (list->vector
     (append (select-imports 'func)
             (map (lambda (def)
                    (cons (func-id def) (func-type def)))
                  (wasm-funcs wasm)))))
  (define globals
    (list->vector
     (append (select-imports 'global)
             (map (lambda (def)
                    (cons (global-id def) (global-type def)))
                  (wasm-globals wasm)))))
  (define tables
    (list->vector
     (append (select-imports 'table)
             (map (lambda (def)
                    (cons (table-id def) (table-type def)))
                  (wasm-tables wasm)))))
  (define tags
    (list->vector
     (map (lambda (def)
            (cons (tag-id def) (tag-type def)))
          (wasm-tags wasm))))
  (define locals
    (match func
      (($ <func>
          id
          ($ <type-use>
             _
             ($ <type>
                _
                ($ <func-sig>
                   (($ <param> param-id param-type) ...)
                   (result-type ...))))
          (($ <local> local-id local-type) ...)
          body)
       (list->vector
        (append (map cons param-id param-type)
                (map cons local-id local-type))))))
  (%make-func-info types funcs globals tables tags locals))

(define (initial-ctx module func)
  (match func
    (($ <func> _ ($ <type-use> _ ($ <type> _ ($ <func-sig> _ results))))
     (make-ctx (make-func-info module func)
               (make-block #f results results #f)
               '()))))

(define* (push-block ctx id param-types result-types #:key is-loop?)
  (match ctx
    (($ <ctx> info block _)
     (let ((branch-arg-types (if is-loop? param-types result-types)))
       (make-ctx info
                 (make-block id branch-arg-types result-types block)
                 param-types)))))

(define (peek ctx)
  (match ctx
    ((or ($ <ctx> _ _ stack)
         ($ <unreachable-ctx> _ _ stack))
     (match stack
       ((top . stack) top)
       (() #f)))
    (($ <invalid-ctx>) #f)))

(define (vector-assq v k)
  (let lp ((i 0))
    (and (< i (vector-length v))
         (let ((pair (vector-ref v i)))
           (if (eq? k (car pair))
               pair
               (lp (1+ i)))))))
(define (vector-lookup v k)
  (if (integer? k)
      (vector-ref v k)
      (vector-assq v k)))

(define (ctx-info-lookup ctx getter def)
  (match ctx
    (($ <ctx> info)
     (cdr (vector-lookup (getter info) def)))
    (($ <unreachable-ctx> info)
     (cdr (vector-lookup (getter info) def)))))

(define (lookup-type ctx def)
  (ctx-info-lookup ctx func-info-types def))
(define (lookup-func-type-use ctx def)
  (ctx-info-lookup ctx func-info-funcs def))
(define (lookup-global ctx def)
  (ctx-info-lookup ctx func-info-globals def))
(define (lookup-table ctx def)
  (ctx-info-lookup ctx func-info-tables def))
(define (lookup-tag ctx def)
  (type-use-sig (ctx-info-lookup ctx func-info-tags def)))
(define (lookup-local ctx def)
  (ctx-info-lookup ctx func-info-locals def))

(define (lookup-func-sig ctx def)
  (match (lookup-type ctx def)
    (($ <sub-type> _ (and sig ($ <func-sig>))) sig)
    ((and sig ($ <func-sig>)) sig)
    (x (error "unexpected type" def x))))

(define (lookup-struct-fields ctx def)
  (match (lookup-type ctx def)
    (($ <sub-type> _ _ ($ <struct-type> fields)) fields)
    (($ <struct-type> fields) fields)))
(define (lookup-struct-field-types ctx struct-type)
  (map field-type (lookup-struct-fields ctx struct-type)))
(define (lookup-struct-field-type ctx struct-type field)
  (match (lookup-struct-fields ctx struct-type)
    ((($ <field> id mutable? type) ...)
     (vector-lookup (list->vector type) field))))

(define (lookup-array-type ctx def)
  (match (lookup-type ctx def)
    (($ <sub-type> _ ($ <array-type> mutable? type)) type)
    (($ <array-type> mutable? type) type)))

(define (lookup-return-type ctx)
  (let lp ((block (ctx-block ctx)))
    (cond
     ((block-parent block) => lp)
     (else (block-branch-arg-types block)))))

(define (compute-stack-effect ctx inst)
  (define (-> params results)
    (make-stack-effect params results #f))
  (define (branch-arg-types target)
    (match ctx
      ((or ($ <ctx> _ block) ($ <unreachable-ctx> _ block))
       (if (integer? target)
           (let lp ((block block) (target target))
             (match block
               (($ <block> id types _ parent)
                (if (zero? target)
                    types
                    (lp parent (1- target))))))
           (let lp ((block block))
             (match block
               (($ <block> id types _ parent)
                (if (eq? target id)
                    types
                    (lp parent)))))))))
  (define (block-stack-effect type)
    (match type
      (#f (-> '() '()))
      ;; Lookup signature by index in func info.
      ((? exact-integer? idx)
       (match ctx
         ((or ($ <ctx> ($ <func-info> types))
              ($ <unreachable-ctx> ($ <func-info> types)))
          (match (vector-ref types idx)
            ((_ . ($ <func-sig> (($ <param> _ params) ...) results))
             (-> params results))))))
      (($ <type-use> _
          ($ <type> _
             ($ <func-sig> (($ <param> _ params) ...) results)))
       (-> params results))
      ((or (? symbol?) ($ <ref-type>))
       (-> '() (list type)))))
  (define (global-type global)
    (match (lookup-global ctx global)
      (($ <global-type> mutable? type) type)))
  (define (table-type def)
    (match (lookup-table ctx def)
      (($ <table-type> limits elem-type) elem-type)))
  (match inst
    ((op . args)
     (match op
       ('unreachable (-> '() #f))
       ('nop (-> '() '()))

       ((or 'block 'loop 'try 'try_delegate)
        (match args
          ((label type . _)
           (block-stack-effect type))))
       ('if
        (match args
          ((label type _ _)
           (match (block-stack-effect type)
             (($ <stack-effect> params results)
              (-> (append params '(i32)) results))))))

       ('throw
        (match args
          ((tag)
           (match (lookup-tag ctx tag)
             (($ <func-sig> (($ <param> id type) ...) ())
              (-> type #f))))))
       ('rethrow
        (-> '() #f))
       ('br
        (match args
          ((target)
           (-> (branch-arg-types target) #f))))
       ('br_if
        (match args
          ((target)
           (let ((types (branch-arg-types target)))
             (-> (append types '(i32)) types)))))
       ('br_table
        (match args
          ((_ target)
           (-> (append (branch-arg-types target) '(i32)) #f))))
       ('return
        (-> (lookup-return-type ctx) #f))

       ('call
        (match args
          ((callee)
           (match (lookup-func-type-use ctx callee)
             (($ <type-use> _
                 ($ <type> _
                    ($ <func-sig> (($ <param> id type) ...) results)))
              (-> type results))))))
       ('call_indirect
        (match args
          ((table type)
           (match (lookup-func-sig ctx type)
             (($ <func-sig> (($ <param> id type) ...) results)
              (-> (append type '(i32)) results))))))
       ('return_call
        (match args
          ((callee)
           (match (lookup-func-type-use ctx callee)
             (($ <type-use> _
                 ($ <type> _
                    ($ <func-sig> (($ <param> id type) ...) results)))
              (-> type #f))))))
       ('return_call_indirect
        (match args
          ((type)
           (match (lookup-func-sig ctx type)
             (($ <func-sig> (($ <param> id type) ...) results)
              (-> (append type '(i32)) #f))))))
       ('call_ref
        (match args
          ((type)
           (match (lookup-func-sig ctx type)
             (($ <func-sig> (($ <param> id params) ...) results)
              (-> (append params (list (make-ref-type #t type))) results))))))
       ('return_call_ref
        (match args
          ((type)
           (match (lookup-func-sig ctx type)
             (($ <func-sig> (($ <param> id params) ...) results)
              (-> (append params (list (make-ref-type #t type))) #f))))))

       ('drop (-> (list (peek ctx)) '()))
       ('select (match args
                  (()
                   (let ((top (peek ctx)))
                     (-> (list top top 'i32) (list top))))
                  ((type ...)
                   (-> (append type type '(i32)) type))))

       ('local.get (match args
                     ((local)
                      (let ((type (lookup-local ctx local)))
                        (-> '() (list type))))))
       ('local.set (match args
                     ((local)
                      (let ((type (lookup-local ctx local)))
                        (-> (list type) '())))))
       ('local.tee (match args
                     ((local)
                      (let ((type (lookup-local ctx local)))
                        (-> (list type) (list type))))))

       ('global.get (match args
                      ((global)
                       (-> '() (list (global-type global))))))
       ('global.set (match args
                      ((global)
                       (-> (list (global-type global)) '()))))

       ('table.get  (match args
                      ((table)
                       (-> '(i32) (list (table-type table))))))
       ('table.set  (match args
                      ((table)
                       (-> (list 'i32 (table-type table)) '()))))
       ('table.size (-> '() '(i32)))
       ('table.init (-> '(i32 i32 i32) '()))
       ('table.copy (-> '(i32 i32 i32) '()))
       ('table.fill (match args
                      ((table)
                       (-> (list 'i32 (table-type table) 'i32) '()))))
       ('table.grow (match args
                      ((table)
                       (-> (list (table-type table) 'i32) '(i32)))))
       ('elem.drop  (-> '() '()))

       ('memory.size (-> '() '(i32)))
       ('memory.grow (-> '(i32) '(i32)))
       ('memory.fill (-> '(i32 i32 i32) '()))
       ('memory.copy (-> '(i32 i32 i32) '()))
       ('memory.init (-> '(i32 i32 i32) '()))
       ('data.drop (-> '() '()))

       ('i32.const (-> '() '(i32)))
       ('i64.const (-> '() '(i64)))
       ('f32.const (-> '() '(f32)))
       ('f64.const (-> '() '(f64)))

       ((or 'i32.load
            'i32.load8_s 'i32.load8_u 'i32.load16_s 'i32.load16_u)
        (-> '(i32) '(i32)))

       ((or 'i64.load
            'i64.load8_s 'i64.load8_u 'i64.load16_s 'i64.load16_u
            'i64.load32_s 'i64.load32_u)
        (-> '(i32) '(i64)))

       ('f32.load (-> '(i32) '(f32)))
       ('f64.load (-> '(i32) '(f64)))

       ((or 'i32.store 'i32.store8 'i32.store16)
        (-> '(i32 i32) '()))
       ((or 'i64.store 'i64.store8 'i64.store16 'i64.store32)
        (-> '(i32 i64) '()))
       ('f32.store (-> '(i32 f32) '()))
       ('f64.store (-> '(i32 f64) '()))

       ('i32.eqz (-> '(i32) '(i32)))
       ((or 'i32.eq 'i32.ne 'i32.lt_s 'i32.lt_u 'i32.gt_s
            'i32.gt_u 'i32.le_s 'i32.le_u 'i32.ge_s 'i32.ge_u)
        (-> '(i32 i32) '(i32)))
       ('i64.eqz (-> '(i64) '(i32)))
       ((or 'i64.eq 'i64.ne 'i64.lt_s 'i64.lt_u 'i64.gt_s
            'i64.gt_u 'i64.le_s 'i64.le_u 'i64.ge_s 'i64.ge_u)
        (-> '(i64 i64) '(i32)))

       ((or 'f32.eq 'f32.ne 'f32.lt 'f32.gt 'f32.le 'f32.ge)
        (-> '(f32 f32) '(i32)))
       ((or 'f64.eq 'f64.ne 'f64.lt 'f64.gt 'f64.le 'f64.ge)
        (-> '(f64 f64) '(i32)))

       ((or 'i32.clz 'i32.ctz 'i32.popcnt
            'i32.extend8_s 'i32.extend16_s)
        (-> '(i32) '(i32)))
       ((or 'i32.add 'i32.sub 'i32.mul 'i32.div_s 'i32.div_u
            'i32.rem_s 'i32.rem_u
            'i32.and 'i32.or 'i32.xor 'i32.shl 'i32.shr_s 'i32.shr_u
            'i32.rotl 'i32.rotr)
        (-> '(i32 i32) '(i32)))
       ('i32.wrap_i64
        (-> '(i64) '(i32)))
       ((or 'i32.trunc_f32_s 'i32.trunc_f32_u
            'i32.trunc_sat_f32_s 'i32.trunc_sat_f32_u
            'i32.reinterpret_f32)
        (-> '(f32) '(i32)))
       ((or 'i32.trunc_f64_s 'i32.trunc_f64_u
            'i32.trunc_sat_f64_s 'i32.trunc_sat_f64_u)
        (-> '(f64) '(i32)))

       ((or 'i64.clz 'i64.ctz 'i64.popcnt
            'i64.extend8_s 'i64.extend16_s 'i64.extend32_s)
        (-> '(i64) '(i64)))
       ((or 'i64.add 'i64.sub 'i64.mul 'i64.div_s 'i64.div_u
            'i64.rem_s 'i64.rem_u
            'i64.and 'i64.or 'i64.xor 'i64.shl 'i64.shr_s 'i64.shr_u
            'i64.rotl 'i64.rotr)
        (-> '(i64 i64) '(i64)))
       ((or 'i64.extend_i32_s 'i64.extend_i32_u)
        (-> '(i32) '(i64)))
       ((or 'i64.trunc_f32_s 'i64.trunc_f32_u
            'i64.trunc_sat_f32_s 'i64.trunc_sat_f32_u)
        (-> '(f32) '(i64)))
       ((or 'i64.trunc_f64_s 'i64.trunc_f64_u
            'i64.trunc_sat_f64_s 'i64.trunc_sat_f64_u
            'i64.reinterpret_f64)
        (-> '(f64) '(i64)))

       ((or 'f32.abs 'f32.neg 'f32.ceil 'f32.floor 'f32.trunc 'f32.nearest
            'f32.sqrt)
        (-> '(f32) '(f32)))
       ((or 'f32.add 'f32.sub 'f32.mul 'f32.div 'f32.min 'f32.max
            'f32.copysign)
        (-> '(f32 f32) '(f32)))
       ((or 'f32.convert_i32_s 'f32.convert_i32_u
            'f32.reinterpret_i32)
        (-> '(i32) '(f32)))
       ((or 'f32.convert_i64_s 'f32.convert_i64_u)
        (-> '(i64) '(f32)))
       ('f32.demote_f64
        (-> '(f64) '(f32)))

       ((or 'f64.abs 'f64.neg 'f64.ceil 'f64.floor 'f64.trunc 'f64.nearest
            'f64.sqrt)
        (-> '(f64) '(f64)))
       ((or 'f64.add 'f64.sub 'f64.mul 'f64.div 'f64.min 'f64.max
            'f64.copysign)
        (-> '(f64 f64) '(f64)))
       ((or 'f64.convert_i32_s 'f64.convert_i32_u)
        (-> '(i32) '(f64)))
       ((or 'f64.convert_i64_s 'f64.convert_i64_u
            'f64.reinterpret_i64)
        (-> '(i64) '(f64)))
       ('f64.promote_f32
        (-> '(f32) '(f64)))

       ('ref.null
        (match args
          ((ht)
           (-> '() (list (make-ref-type #t ht))))))
       ((or 'ref.is_null 'ref.test)
        (-> (list (peek ctx)) '(i32)))
       ('ref.eq
        (-> (list (make-ref-type #t 'eq) (make-ref-type #t 'eq)) '(i32)))
       ('ref.func
        (match args
          ((callee)
           (match (lookup-func-type-use ctx callee)
             (($ <type-use> id sig)
              (-> '() (list (make-ref-type #f id))))))))
       ('ref.as_non_null
        (match (peek ctx)
          ((and top ($ <ref-type> nullable? ht))
           (-> (list top)
               (list (make-ref-type #f ht))))))
       ('ref.cast
        (match args
          ((_ ht)
           (match (peek ctx)
             ((and top ($ <ref-type> nullable? ht*))
              (-> (list top) (list (make-ref-type #f ht))))))))
       ('br_on_null
        (match args
          ((target)
           (let ((types (branch-arg-types target)))
             (match (peek ctx)
               ((and top ($ <ref-type> nullable? ht))
                (-> (append types (list top))
                    (append types (list (make-ref-type #f ht))))))))))
       ('br_on_non_null
        (match args
          ((target)
           (let ((top (peek ctx)))
             (match (branch-arg-types target)
               ((types* ... _)
                (-> (append types* (list top))
                    types*)))))))
       ((or 'br_on_cast 'br_on_cast_fail)
        ;; FIXME: The rest of the Hoot wasm toolchain needs to switch
        ;; to the two-type form of br_on_cast / br_on_cast_fail
        (match args
          ((target rt1 rt2)
           (let ((types (branch-arg-types target)))
             (-> (append types (list rt1))
                 (append types (list (if (eq? op 'br_on_cast) rt1 rt2))))))))

       ('struct.get
        (match args
          ((ht field)
           (-> (list (make-ref-type #t ht))
               (list (lookup-struct-field-type ctx ht field))))))
       ((or 'struct.get_s 'struct.get_u)
        (match args
          ((ht field)
           (-> (list (make-ref-type #t ht)) '(i32)))))
       ('struct.set
        (match args
          ((ht field)
           (-> (list (make-ref-type #t ht)
                     (lookup-struct-field-type ctx ht field))
               '()))))
       ('struct.new
        (match args
          ((ht)
           (-> (lookup-struct-field-types ctx ht)
               (list (make-ref-type #f ht))))))
       ('struct.new_default
        (match args
          ((ht)
           (-> '() (list (make-ref-type #f ht))))))

       ('array.get
        (match args
          ((ht)
           (-> (list (make-ref-type #t ht) 'i32)
               (list (lookup-array-type ctx ht))))))
       ((or 'array.get_s 'array.get_u)
        (match args
          ((ht)
           (-> (list (make-ref-type #t ht) 'i32) '(i32)))))
       ('array.set
        (match args
          ((ht)
           (-> (list (make-ref-type #t ht) 'i32 (lookup-array-type ctx ht))
               '()))))
       ('array.fill
        (match args
          ((ht)
           (-> (list (make-ref-type #t ht) 'i32 (lookup-array-type ctx ht) 'i32)
               '()))))
       ('array.copy
        (match args
          ((ht1 ht2)
           (-> (list (make-ref-type #t ht1) 'i32
                     (make-ref-type #t ht2) 'i32 'i32)
               '()))))
       ('array.len
        (-> (list (make-ref-type #t 'array)) '(i32)))
       ('array.new
        (match args
          ((ht)
           (-> (list (lookup-array-type ctx ht) 'i32)
               (list (make-ref-type #f ht))))))
       ('array.new_fixed
        (match args
          ((ht len)
           (-> (make-list len (lookup-array-type ctx ht))
               (list (make-ref-type #f ht))))))
       ('array.new_default
        (match args
          ((ht)
           (-> '(i32) (list (make-ref-type #f ht))))))
       ((or 'array.new_data 'array.new_elem)
        (match args
          ((ht)
           (-> '(i32 i32) (list (make-ref-type #f ht))))))

       ('i31.new
        (-> '(i32) (list (make-ref-type #f 'i31))))
       ((or 'i31.get_s 'i31.get_u)
        (-> (list (make-ref-type #f 'i31)) '(i32)))

       ('extern.internalize
        (match (peek ctx)
          (($ <ref-type> nullable? _)
           (-> (list (make-ref-type nullable? 'extern))
               (list (make-ref-type nullable? 'any))))))
       ('extern.externalize
        (match (peek ctx)
          (($ <ref-type> nullable? _)
           (-> (list (make-ref-type nullable? 'any))
               (list (make-ref-type nullable? 'extern))))))

       ((or 'string.new_utf8 'string.new_lossy_utf8 'string.new_wtf8
            'string.new_wtf16)
        (-> '(i32 i32)
            (list (make-ref-type #f 'string))))
       ((or 'string.new_utf8_array 'string.new_lossy_utf8_array
            'string.new_wtf8_array)
        (-> (list (make-ref-type #t '$i8-array) 'i32 'i32)
            (list (make-ref-type #f 'string))))
       ((or 'string.new_wtf16_array)
        (-> (list (make-ref-type #t '$i16-array) 'i32 'i32)
            (list (make-ref-type #f 'string))))
       ((or 'string.measure_utf8 'string.measure_wtf8
            'string.measure_wtf16)
        (-> (list (make-ref-type #t 'string))
            '(i32)))
       ((or 'string.encode_utf8 'string.encode_lossy_utf8 'string.encode_wtf8
            'string.encode_wtf16)
        (-> (list (make-ref-type #t 'string) 'i32)
            '(i32)))
       ((or 'string.encode_utf8_array 'string.encode_lossy_utf8_array
            'string.encode_wtf8_array)
        (-> (list (make-ref-type #t 'string)
                  (make-ref-type #t '$i8-array)
                  'i32)
            '(i32)))
       ('string.encode_wtf16_array
        (-> (list (make-ref-type #t 'string)
                  (make-ref-type #t '$i16-array)
                  'i32)
            '(i32)))
       ('string.const
        (-> '() (list (make-ref-type #f 'string))))
       ('string.concat
        (-> (list (make-ref-type #t 'string)
                  (make-ref-type #t 'string))
            (list (make-ref-type #f 'string))))
       ((or 'string.eq 'string.compare)
        (-> (list (make-ref-type #t 'string)
                  (make-ref-type #t 'string))
            '(i32)))
       ('string.is_usv_sequence
        (-> (list (make-ref-type #t 'string))
            '(i32)))
       ('string.from_code_point
        (-> (list 'i32)
            (list (make-ref-type #f 'string))))

       ('string.as_wtf8
        (-> (list (make-ref-type #t 'string))
            (list (make-ref-type #f 'stringview_wtf8))))
       ((or 'stringview_wtf8.encode_utf8
            'stringview_wtf8.encode_lossy_utf8
            'stringview_wtf8.encode_wtf8)
        (-> (list (make-ref-type #t 'stringview_wtf8)
                  'i32 'i32 'i32)
            '(i32 i32)))
       ('stringview_wtf8.advance
        (-> (list (make-ref-type #t 'stringview_wtf8)
                  'i32 'i32)
            '(i32)))
       ('stringview_wtf8.slice
        (-> (list (make-ref-type #t 'stringview_wtf8)
                  'i32 'i32)
            (list (make-ref-type #f 'string))))

       ('string.as_wtf16
        (-> (list (make-ref-type #t 'string))
            (list (make-ref-type #f 'stringview_wtf16))))
       ('stringview_wtf16.length
        (-> (list (make-ref-type #t 'stringview_wtf16))
            '(i32)))
       ('stringview_wtf16.get_codeunit
        (-> (list (make-ref-type #t 'stringview_wtf16) 'i32)
            '(i32)))
       ('stringview_wtf16.encode
        (-> (list (make-ref-type #t 'stringview_wtf16) 'i32 'i32 'i32)
            '(i32)))
       ('stringview_wtf16.slice
        (-> (list (make-ref-type #t 'stringview_wtf16)
                  'i32 'i32)
            (list (make-ref-type #f 'string))))

       ('string.as_iter
        (-> (list (make-ref-type #t 'string))
            (list (make-ref-type #f 'stringview_iter))))
       ('stringview_iter.next
        (-> (list (make-ref-type #t 'stringview_iter))
            '(i32)))
       ((or 'stringview_iter.advance 'stringview_iter.rewind)
        (-> (list (make-ref-type #t 'stringview_iter) 'i32)
            '(i32)))
       ('stringview_iter.slice
        (-> (list (make-ref-type #t 'stringview_iter)
                  'i32)
            (list (make-ref-type #f 'string))))

       ((or 'i8x16.splat 'i16x8.splat 'i32x4.splat)
        (-> '(i32) '(i128)))
       ('i64x2.splat (-> '(i64) '(i128)))
       ('f32x4.splat (-> '(f32) '(i128)))
       ('f64x2.splat (-> '(f64) '(i128)))

       (_ (error "unhandled instruction" op))))))

(define (apply-stack-effect ctx effect)
  (define (heap-type-sub-type? sub super)
    (or (eq? sub super)
        (let lp ((sub (if (symbol? sub) sub (lookup-type ctx sub))))
          (match sub
            ('i31 (memq super '(i31 eq)))
            (($ <sub-type> _ supers type)
             (or (and supers (memq super supers))
                 (lp type)))
            (($ <array-type>)
             (memq super '(array eq any)))
            (($ <struct-type>)
             (memq super '(struct eq any)))
            (($ <func-sig>)
             (eq? super 'func))))))
  (define (is-subtype? sub super)
    (cond
     ((eq? sub super) #t)
     ((and (eq? sub 'i32) (memq super '(i32 i16 i8))) #t)
     ((and (ref-type? sub) (ref-type? super))
      (and (or (ref-type-nullable? super)
               (not (ref-type-nullable? sub)))
           (heap-type-sub-type? (ref-type-heap-type sub)
                                (ref-type-heap-type super))))
     ;; The funcref type works for any function reference.
     ((and (eq? super 'funcref) (ref-type? sub)
           (heap-type-sub-type? (ref-type-heap-type sub) 'func))
      #t)
     (else #f)))

  (match ctx
    (($ <invalid-ctx>) ctx)
    (($ <unreachable-ctx> info block stack)
     (match effect
       (($ <stack-effect> params results block-end?)
        (let lp ((params (reverse params)) (stack stack))
          (match params
            ((param . params)
             (match stack
               ;; The bottom of the unreachable stack is treated as a
               ;; polymorphic stack that contains any type, so there
               ;; is no reason to continue type checking.
               (()
                (lp '() '()))
               ;; Peeking at the unreachable stack may return #f,
               ;; which can stand in for any type.
               ((#f . stack)
                (lp params stack))
               ;; A proper type is on top of the stack, type checking
               ;; happens the same as in <ctx>.
               ((top . stack)
                (if (is-subtype? top param)
                    (lp params stack)
                    (make-invalid-ctx
                     (format #f "expected ~a, got ~a" param top))))))
            (()
             (if (and block-end? (not (null? stack)))
                 (make-invalid-ctx
                  (format #f "extra values on stack at block end ~a" stack))
                 (match results
                   (#f (make-unreachable-ctx info block '()))
                   ((result ...)
                    (make-unreachable-ctx info block (append (reverse result) stack)))))))))))
    (($ <ctx> info block stack)
     (match effect
       (($ <stack-effect> params results block-end?)
        (let lp ((params (reverse params)) (stack stack))
          (match params
            ((param . params)
             (match stack
               (()
                (make-invalid-ctx
                 (format #f "expected ~a, got empty stack" param)))
               ((top . stack)
                (if (is-subtype? top param)
                    (lp params stack)
                    ;; FIXME: more info here.
                    (make-invalid-ctx
                     (format #f "expected ~a, got ~a" param top))))))
            (()
             (if (and block-end? (not (null? stack)))
                 (make-invalid-ctx
                  (format #f "extra values on stack at block end ~a" stack))
                 (match results
                   (#f (make-unreachable-ctx info block '()))
                   ((result ...)
                    (make-ctx info block (append (reverse result) stack)))))))))))))

(define (fallthrough ctx)
  (let ((types
         (match ctx
           (($ <unreachable-ctx> _ ($ <block> _ _ types)) types)
           (($ <ctx> _ ($ <block> _ _ types)) types))))
    (apply-stack-effect ctx (make-stack-effect types #f #t))))
