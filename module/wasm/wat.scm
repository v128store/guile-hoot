;;; WebAssembly text format parser and unparser
;;; Copyright (C) 2023 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
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
;;; Converts WAT to WASM and vice versa.
;;;
;;; Code:

(define-module (wasm wat)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (append-map filter-map))
  #:use-module (srfi srfi-11)
  #:use-module (wasm types)
  #:export (wat->wasm wasm->wat))

;; to-do:
;;  - support reftypes
;;  - support bulk memory instructions
;;  - tail calls
;;  - stringref

;; differences from standard: scheme comments / no block comments.
;; strings have guile string syntax; bytevectors also for data.  could
;; write standard-compliant parser instead (port from wassemble).

(define (natural-alignment inst)
  (case inst
    ((i32.load8_s
      i32.load8_u
      i64.load8_s
      i64.load8_u
      i32.store8
      i64.store8)
     0)
    ((i32.load16_s
      i32.load16_u
      i64.load16_s
      i64.load16_u
      i32.store16
      i64.store16)
     1)
    ((i32.load
      f32.load
      i64.load32_s
      i64.load32_u
      i32.store
      f32.store
      i64.store32)
     2)
    ((i64.load
      f64.load
      i64.store
      f64.store)
     3)
    (else (error "unrecognized instruction" inst))))

(define (wat->wasm expr)
  (define (id? x)
    (and (symbol? x) (eqv? (string-ref (symbol->string x) 0) #\$)))
  (define (kw? x)
    (and (symbol? x) (not (id? x))))
  (define (u32? x)
    (and (exact-integer? x) (<= 0 x (1- (ash 1 32)))))
  (define (s32? x)
    (and (exact-integer? x) (<= (ash -1 31) x (1- (ash 1 31)))))
  (define (u64? x)
    (and (exact-integer? x) (<= 0 x (1- (ash 1 64)))))
  (define (s64? x)
    (and (exact-integer? x) (<= (ash -1 63) x (1- (ash 1 63)))))

  (define (id-or-idx? x) (or (id? x) (u32? x)))

  (define (assert-true x msg)
    (or x (error msg)))

  ;; Identifiers with a space can't be in the source program.
  (define fresh-idx 0)
  (define (fresh-id!)
    (let ((id (string->symbol
               (string-append "$fresh " (number->string fresh-idx)))))
      (set! fresh-idx (1+ fresh-idx))
      id))

  (define (partition-clauses x)
    (define id #f)
    (define types '())
    (define imports '())
    (define funcs '())
    (define tables '())
    (define memories '())
    (define globals '())
    (define exports '())
    (define start #f)
    (define elems '())
    (define datas '())
    (define tags '())
    (define strings '())
    (define (collect-raw x)
      (match x
        (('type . x) (set! types (cons x types)))
        (('import . x) (set! imports (cons x imports)))
        (('func . x) (set! funcs (cons x funcs)))
        (('table . x) (set! tables (cons x tables)))
        (('memory . x) (set! memories (cons x memories)))
        (('global . x) (set! globals (cons x globals)))
        (('export . x) (set! exports (cons x exports)))
        (('start . x) (begin
                        (when start (error "multiple start clauses"))
                        (set! start x)))
        (('elem . x) (set! elems (cons x elems)))
        (('data . x) (set! datas (cons x datas)))
        (('tag . x) (set! tags (cons x tags)))
        (('strings . x) (set! strings (append strings x)))
        (('rec . _) (set! types (cons x types)))
        (_ (error "unexpected form in module" x))))
    (match x
      (('module (? id? mod-id) . clauses)
       (set! id mod-id)
       (for-each collect-raw clauses))
      (('module . clauses) (for-each collect-raw clauses))
      ((clauses ...) (for-each collect-raw clauses))
      (_ (error "unexpected module" x)))
    (make-wasm id (reverse types) (reverse imports) (reverse funcs)
               (reverse tables) (reverse memories) (reverse globals)
               (reverse exports) start (reverse elems)
               (reverse datas) (reverse tags) strings '()))

  (define (valid-heap-type? x)
    (match x
      ((or 'func 'extern 'any 'none 'noextern 'nofunc 'eq 'struct 'array 'i31
           'string 'stringview_iter 'stringview_wtf8 'stringview_wtf16
           (? id-or-idx?))
       #t)
      (_ #f)))
  (define (parse-heap-type x)
    (if (valid-heap-type? x)
        x
        (error "bad heaptype" x)))
  (define* (parse-ref-type x #:key (error-message "bad reftype"))
    (match x
      (('ref 'null ht) (make-ref-type #t (parse-heap-type ht)))
      (('ref ht) (make-ref-type #f (parse-heap-type ht)))
      ('funcref (make-ref-type #t 'func))
      ('externref (make-ref-type #t 'extern))
      ('anyref (make-ref-type #t 'any))
      ('nullref (make-ref-type #t 'none))
      ('nullexternref (make-ref-type #t 'noextern))
      ('nullfuncref (make-ref-type #t 'nofunc))
      ('eqref (make-ref-type #t 'eq))
      ('structref (make-ref-type #t 'struct))
      ('arrayref (make-ref-type #t 'array))
      ('i31ref (make-ref-type #t 'i31))
      (_ (error error-message x))))
  (define (parse-val-type x)
    (match x
      ((or 'i32 'i64 'f32 'f64 'v128) x)
      (_ (parse-ref-type x #:error-message "bad valtype"))))
  (define (parse-storage-type x)
    (match x
      ((or 'i8 'i16) x)
      (_ (parse-val-type x))))
  (define (parse-params x)
    (match x
      (((? id? id) vt) (list (make-param id (parse-val-type vt))))
      ((vt ...) (map (lambda (vt) (make-param #f (parse-val-type vt))) vt))))
  (define (parse-results x)
    (match x
      ((vt ...) (map parse-val-type vt))))
  (define (parse-func-sig x)
    (let lp ((x x) (params '()))
      (match x
        ((('param . tail) . x)
         (lp x (append params (parse-params tail))))
        (_
         (let lp ((x x) (results '()))
           (match x
             ((('result . tail) . x)
              (lp x (append results (parse-results tail))))
             (_ (values (make-func-sig params results) x))))))))
  (define (parse-func-type x)
    (let-values (((sig tail) (parse-func-sig x)))
      (unless (null? tail) "unexpected tail on func type" tail)
      sig))
  (define (parse-type-use x)
    (define (parse-sig idx x)
      (let-values (((sig x) (parse-func-sig x)))
        (values (make-type-use idx sig) x)))
    (match x
      ((('type idx) . sig)
       (parse-sig idx sig))
      (sig (parse-sig #f sig))))
  (define (parse-block-type x)
    (match x
      ;; cf. emit-block-type in (@ (wasm assemble) assemble-wasm)
      (((and t (or (? symbol?) ('ref . _))) . tail)
       (values
        (make-type-use #f (make-func-sig '() (list (parse-val-type t))))
        tail))
      (_ (parse-type-use x))))
  (define (parse-limits x)
    (match x
      (((? u32? min) (? u32? max) . tail)
       (values (make-limits min max) tail))
      (((? u32? min) . tail)
       (values (make-limits min #f) tail))))
  (define (parse-elem-type x)
    (match x
      (('funcref) 'funcref)
      ((x) (parse-ref-type x #:error-message "bad elem type"))
      (_ (error "bad elem type"))))
  (define (parse-table-type x)
    (let-values (((limits x) (parse-limits x)))
      (make-table-type limits (parse-elem-type x))))
  (define (parse-mem-type x)
    (let-values (((limits x) (parse-limits x)))
      (match x
        (() (make-mem-type limits)))))
  (define (parse-global-type x)
    (match x
      (('mut vt) (make-global-type #t (parse-val-type vt)))
      (vt (make-global-type #f (parse-val-type vt)))))
  (define (parse-id-or-idx x)
    (match x
      (((? id-or-idx? id) . x) (values id x))
      (_ (values #f x))))
  (define (parse-id x)
    (match x
      (((? id? id) . x) (values id x))
      (_ (values #f x))))
  (define (parse-array-type x)
    (match x
      (('mut t) (make-array-type #t (parse-storage-type t)))
      (t (make-array-type #f (parse-storage-type t)))))
  (define (parse-field x)
    (match x
      (('field (? id? id) ('mut t))
       (make-field id #t (parse-storage-type t)))
      (('field (? id? id) t)
       (make-field id #f (parse-storage-type t)))
      (('mut t)
       (make-field #f #t (parse-storage-type t)))
      (t
       (make-field #f #f (parse-storage-type t)))))
  (define (parse-struct-type x)
    (make-struct-type (map parse-field x)))
  (define (parse-sub-type type)
    (match type
      (('sub 'final (? id-or-idx? super) ... type)
       (make-sub-type #t super (parse-prim-type type)))
      (('sub (? id-or-idx? super) ... type)
       (make-sub-type #f super (parse-prim-type type)))
      (type
       (parse-prim-type type))))
  (define (parse-prim-type x)
    (match x
      (('func . sig) (parse-func-sig sig))
      (('array sig) (parse-array-type sig))
      (('struct . sig) (parse-struct-type sig))))
  (define (parse-type x)
    (match x
      (('sub id sub)
       (make-sub-type #f (list id) (parse-prim-type sub)))
      (_ (parse-prim-type x))))
  (define (parse-type-def def)
    (define (parse-def def)
      (match def
          (((? id-or-idx? id) type) (make-type id (parse-sub-type type)))
          ((type) (make-type #f (parse-sub-type type)))))
    (match def
      (('rec ('type . def) ...) (make-rec-group (map parse-def def)))
      (def (parse-def def))))
  (define (parse-import x)
    (define (parse-inner mod name kind id tail)
      (match kind
        ('func (make-import mod name 'func id (parse-type-use tail)))
        ('table (make-import mod name 'table id (parse-table-type tail)))
        ('memory (make-import mod name 'memory id (parse-mem-type tail)))
        ('global (make-import mod name 'global id
                              (match tail
                                ((type) (parse-global-type type)))))))
    (match x
      (((? string? mod) (? string? name) desc)
       (match desc
         ((kind (? id? id) . tail)
          (parse-inner mod name kind id tail))
         ((kind . tail)
          (parse-inner mod name kind #f tail))))))
  (define (parse-export x)
    (match x
      (((? string? name) ((and kind (or 'func 'table 'memory 'global)) idx))
       (make-export name kind idx))))
  (define (parse-mem-arg x inst)
    (define (symbol-with-prefix prefix)
      (lambda (x)
        (and (symbol? x)
         (string-prefix? prefix (symbol->string x)))))
    (define (symbol-suffix x prefix)
      (substring (symbol->string x) (string-length prefix)))
    (define (parse-arg prefix x)
      (match x
        (((? (symbol-with-prefix prefix) arg) . x)
         (values
          (or (string->number (symbol-suffix arg prefix))
              (error "bad mem arg" arg))
          x))
        (_ (values #f x))))
    (let*-values (((idx x) (parse-id-or-idx x))
                  ((offset x) (parse-arg "offset=" x))
                  ((align x) (parse-arg "align=" x)))
      (values (make-mem-arg (or idx 0)
                            (or offset 0)
                            (or align (natural-alignment inst)))
              x)))
  (define (unfold-instruction inst)
    (define (unparse-val-type type)
      (match type
        ((? symbol?) type)
        (($ <ref-type> #f ht) `(ref ,ht))
        (($ <ref-type> #t ht) `(ref null ,ht))))
    (define (unfold-func-sig sig)
      (match sig
        (($ <func-sig> params results)
         `(,@(map (match-lambda
                   ((#f . vt) `(param ,(unparse-val-type vt)))
                   ((id . vt) `(param ,id ,(unparse-val-type vt))))
                  params)
           (result ,@(map unparse-val-type results))))))
    (define (unfold-type-use type)
      (match type
        (($ <type-use> #f sig)
         (unfold-func-sig sig))
        (($ <type-use> idx sig)
         `((type ,idx) ,@(unfold-func-sig sig)))))
    (define (unfold-mem-arg arg)
      (match arg
        (($ <mem-arg> id offset align)
         `(,@(if (eqv? id 0)
                 '()
                 (list id))
           ,@(if offset
                 (list (string->symbol (format #f "offset=~a" offset)))
                 '())
           ,@(if align
                 (list (string->symbol (format #f "align=~a" align)))
                 '())))))
    (match inst
      (((and tag (or 'loop 'block)) body ...)
       (cons tag (append body (list 'end))))
      (('if . body)
       (let*-values (((label body) (parse-id body))
                     ((type body) (parse-block-type body)))
         (define (finish test consequent alternate)
           `(,@test
             if ,@(if label `(,label) '()) ,@(unfold-type-use type)
             ,@consequent
             else ,@alternate
             end))
         (match body
           ((test ... ('then consequent ...))
            (finish test consequent '()))
           ((test ... ('then consequent ...) ('else alternate ...))
            (finish test consequent alternate)))))
      (((and tag (or 'br 'br_if 'call 'local.get 'local.set 'local.tee
                     'global.get 'global.set))
        idx
        . args)
       `(,@args ,tag ,idx))
      (((and tag (or 'br_on_cast 'br_on_cast_fail)) target rt1 rt2
        . args)
       `(,@args ,tag ,target ,rt1 ,rt2))
      (('br_table . args)
       (let lp ((args args) (targets '()))
         (match args
           (((? id-or-idx? target) . args)
            (lp args (cons target targets)))
           (_ `(,@args br_table ,@(reverse targets))))))
      (('call_indirect . args)
       (let*-values (((table args) (parse-id-or-idx args))
                     ((type args) (parse-type-use args)))
         `(,@args call_indirect ,table ,@(unfold-type-use type))))
      (((and tag (or 'i32.load
                     'i64.load
                     'f32.load
                     'f64.load
                     'i32.load8_s
                     'i32.load8_u
                     'i32.load16_s
                     'i32.load16_u
                     'i64.load8_s
                     'i64.load8_u
                     'i64.load16_s
                     'i64.load16_u
                     'i64.load32_s
                     'i64.load32_u
                     'i32.store
                     'i64.store
                     'f32.store
                     'f64.store
                     'i32.store8
                     'i32.store16
                     'i64.store8
                     'i64.store16
                     'i64.store32))
        . args)
       (let-values (((mem-arg args) (parse-mem-arg args tag)))
         `(,@args ,tag ,@(unfold-mem-arg mem-arg))))
      (((and tag (or 'i32.const 'i64.const 'f32.const 'f64.const)) val . insts)
       `(,@insts ,tag ,val))
      (('ref.func (? id-or-idx? id) . args)
       `(,@args ref.func ,id))
      (((and tag (or 'call_ref 'return_call_ref)) (? id-or-idx? id) . args)
       `(,@args ,tag ,id))
      (((and tag 'ref.null) (? valid-heap-type? id) . args)
       `(,@args ,tag ,id))
      (((and tag (or 'table.set 'table.get 'table.size 'table.grow
                     'table.fill 'elem.drop
                     'memory.size 'memory.grow 'memory.fill))
        (? id-or-idx? id) . args)
       `(,@args ,tag ,id))
      (((and tag (or 'memory.init 'table.init))
        (? id-or-idx? id) (? id-or-idx? eid) . args)
       `(,@args ,tag ,id ,eid))
      (((and tag (or 'memory.init 'table.init)) (? id-or-idx? eid) . args)
       `(,@args ,tag 0 ,eid))
      (((and tag (or 'memory.copy 'table.copy))
        (? id-or-idx? a) (? id-or-idx? b) . args)
       `(,@args ,tag ,a ,b))
      (((and tag (or 'memory.copy 'table.copy)) . args)
       `(,@args ,tag 0 0))
      (((and tag (or 'struct.new 'struct.new_default)) (? id-or-idx? id) . args)
       `(,@args ,tag ,id))
      (((and tag (or 'struct.set 'struct.get 'struct.get_s 'struct.get_u))
        (? id-or-idx? ti)
        (? id-or-idx? fi) . args)
       `(,@args ,tag ,ti ,fi))
      (((and tag (or 'ref.test 'ref.cast)) 'null (? valid-heap-type? id) . args)
       `(,@args ,tag null ,id))
      (((and tag (or 'ref.test 'ref.cast)) (? valid-heap-type? id) . args)
       `(,@args ,tag ,id))
      (((and tag 'string.const) (? string? str) . args)
       `(,@args ,tag ,str))
      (((and tag (or 'array.new 'array.new_default 'array.fill
                     'array.get 'array.set 'array.get_u 'array.get_s))
        (? id-or-idx? ti) . args)
       `(,@args ,tag ,ti))
      (((and tag 'array.new_fixed) (? id-or-idx? ti) (? s32? k) . args)
       `(,@args ,tag ,ti ,k))
      (((and tag (or 'array.copy
                     'array.new_data 'array.new_elem
                     'array.init_data 'array.init_elem))
        (? id-or-idx? ti1) (? id-or-idx? ti2) . args)
       `(,@args ,tag ,ti1 ,ti2))
      (((and tag 'return_call) (? id-or-idx? id) . args)
       `(,@args ,tag ,id))
      ((tag . args)
       `(,@args ,tag))))
  (define (parse-block x block-kind)
    (let lp ((in x) (out '()))
      (define (lp/inst in parsed)
        (lp in (cons parsed out)))
      (define (lp/block block-label in parsed)
        ;; Skip end label.
        (let-values (((label in) (parse-id in)))
          (when label
            (unless (eq? label block-label) (error "bad end label" label)))
          (lp/inst in parsed)))
      (match in
        (()
         (unless (eq? block-kind 'body)
           (error "unexpected end of instruction sequence"))
         (values (reverse out) '()))
        (((folded ...) . in)
         (lp (append (unfold-instruction folded) in) out))
        (((? kw? inst) . in)
         (match inst
           ((or 'block 'loop)
            (let*-values (((label in) (parse-id in))
                          ((type in) (parse-block-type in))
                          ((insts in) (parse-block in inst)))
              (lp/block label in `(,inst ,label ,type ,insts))))
           ('if
            (let*-values (((label in) (parse-id in))
                          ((type in) (parse-block-type in))
                          ((consequent in) (parse-block in 'then)))
              (match in
                (('else . in)
                 (let-values (((alternate in) (parse-block in 'else)))
                   (lp/block label in
                             `(,inst ,label ,type ,consequent ,alternate))))
                (_
                 (lp/block label in `(,inst ,label ,type ,consequent ()))))))
           ('else
            (unless (eq? block-kind 'then)
              (error "unexpected 'else'"))
            (values (reverse out) (cons 'else in)))
           ('end
            (when (eq? block-kind 'body)
              (error "unexpected 'end'"))
            (values (reverse out) in))
           ('ref.func
            (match in
              (((? id-or-idx? id) . in)
               (lp/inst in `(,inst ,id)))))
           ((or 'br 'br_if
                'call 'local.get 'local.set 'local.tee 'global.get
                'global.set)
            (let-values (((idx in) (parse-id-or-idx in)))
              (unless idx (error "missing idx" inst in))
              (lp/inst in `(,inst ,idx))))
           ((or 'br_on_cast 'br_on_cast_fail)
            (match in
              (((? id-or-idx? target) rt1 rt2 . in)
               (lp/inst
                in
                `(,inst ,target ,(parse-ref-type rt1) ,(parse-ref-type rt2))))))
           ('br_table
            (let lp ((in in) (targets '()))
              (match in
                (((? id-or-idx? target) . in)
                 (lp in (cons target targets)))
                (_
                 (match (reverse targets)
                   ((target ... default)
                    (lp/inst in `(,inst ,target ,default)))
                   (_ (error "no targets for br_table")))))))
           ('call_indirect
            (let*-values (((table in) (parse-id-or-idx in))
                          ((type in) (parse-type-use in)))
              (lp/inst in `(,inst ,(or table 0) ,type))))
           ((or 'i32.load 'i64.load
                'f32.load 'f64.load
                'i32.load8_s 'i32.load8_u 'i32.load16_s 'i32.load16_u
                'i64.load8_s 'i64.load8_u 'i64.load16_s 'i64.load16_u
                'i64.load32_s 'i64.load32_u
                'i32.store 'i64.store
                'f32.store 'f64.store
                'i32.store8 'i32.store16
                'i64.store8 'i64.store16 'i64.store32)
            (let-values (((mem-arg in) (parse-mem-arg in inst)))
              (lp/inst in `(,inst ,mem-arg))))
           ('i32.const
            (match in
              (((? s32? const) . in)
               (lp/inst in `(,inst ,const)))))
           ('i64.const
            (match in
              (((? s64? const) . in)
               (lp/inst in `(,inst ,const)))))
           ((or 'f32.const 'f64.const)
            (match in
              (((? real? const) . in)
               (lp/inst in `(,inst ,(exact->inexact const))))))
           ((or 'call_ref 'return_call_ref)
            (match in
              (((? id-or-idx? id) . in)
               (lp/inst in `(,inst ,id)))))
           ('ref.null
            (match in
              ((id . in)
               (lp/inst in `(,inst ,(parse-heap-type id))))))
           ((or 'table.set 'table.get 'table.size 'table.grow
                'table.fill 'elem.drop
                'memory.size 'memory.grow 'memory.fill)
            (match in
              (((? id-or-idx? id) . in)
               (lp/inst in `(,inst ,id)))
              (_
               (lp/inst in `(,inst 0)))))
           ((or 'table.copy 'memory.copy)
            (match in
              (((? id-or-idx? a) (? id-or-idx? b) . in)
               (lp/inst in `(,inst ,a ,b)))
              (_
               (lp/inst in `(,inst 0 0)))))
           ((or 'table.init 'memory.init)
            (match in
              (((? id-or-idx? tid) (? id-or-idx? eid) . in)
               (lp/inst in `(,inst ,tid ,eid)))
              (((? id-or-idx? eid) . in)
               (lp/inst in `(,inst 0 ,eid)))))
           ((or 'struct.new 'struct.new_default)
            (match in
              (((? id-or-idx? id) . in)
               (lp/inst in `(,inst ,id)))))
           ((or 'struct.set 'struct.get 'struct.get_s 'struct.get_u)
            (match in
              (((? id-or-idx? ti) (? id-or-idx? fi) . in)
               (lp/inst in `(,inst ,ti ,fi)))))
           ((or 'ref.test 'ref.cast)
            (match in
              (('null ht . in)
               (lp/inst in `(,inst ,(make-ref-type #t (parse-heap-type ht)))))
              ((ht . in)
               (lp/inst in `(,inst ,(make-ref-type #f (parse-heap-type ht)))))))
           ('string.const
            (match in
              (((? string? str) . in)
               (lp/inst in `(,inst ,str)))))
           ((or 'array.new 'array.new_default 'array.fill
                'array.get 'array.set 'array.get_u 'array.get_s)
            (match in
              (((? id-or-idx? ti) . in)
               (lp/inst in `(,inst ,ti)))))
           ('array.new_fixed
            (match in
              (((? id-or-idx? ti) (? s32? k) . in)
               (lp/inst in `(,inst ,ti ,k)))))
           ((or 'array.copy
                'array.new_data 'array.new_elem
                'array.init_data 'array.init_elem)
            (match in
              (((? id-or-idx? idx1) (? id-or-idx? idx2) . in)
               (lp/inst in `(,inst ,idx1 ,idx2)))))
           ('return_call
            (match in
              (((? id-or-idx? id) . in)
               (lp/inst in `(,inst ,id)))))
           (_
            (lp/inst in (list inst))))))))
  (define (parse-offset x)
    (parse-block (match x
                   (('offset . offset) offset)
                   (offset (list offset)))
                 'body))
  (define (parse-init init)
    (parse-block (match init
                   (('item . init) init)
                   (init (list init)))
                 'body))
  (define (parse-elem x)
    (define (parse-table-use x)
      (match x
        ((('table (? id-or-idx? table)) . x) (values table x))
        (_ (values #f x))))
    (define (parse-elemlist elemlist)
      (match elemlist
        (('func (? id-or-idx? id) ...)
         (values 'funcref (map (lambda (id) `((ref.func ,id))) id)))
        (((? id-or-idx? id) ...)
         (values 'funcref (map (lambda (id) `((ref.func ,id))) id)))
        ((type init ...)
         (values (parse-ref-type type) (map parse-init init)))))
    (let-values (((id x) (parse-id x)))
      (match x
        (('declare . elemlist)
         ;; Declarative element segment.
         (let-values (((type inits) (parse-elemlist elemlist)))
           (make-elem id 'declarative #f type #f inits)))
        ((('table table) offset . elemlist)
         ;; Active element segment with explicit table.
         (let-values (((type inits) (parse-elemlist elemlist)))
           (make-elem id 'active table type (parse-offset offset) inits)))
        (((and offset (or ('offset . _) ('i32.const _))) . elemlist)
         ;; Active element segment for table 0.
         (let-values (((type inits) (parse-elemlist elemlist)))
           (make-elem id 'active 0 type (parse-offset offset) inits)))
        (elemlist
         (let-values (((type inits) (parse-elemlist elemlist)))
           (make-elem id 'passive #f type #f inits))))))
  (define (bytevector-concatenate bvs)
    (call-with-output-bytevector
     (lambda (p)
       (for-each (lambda (bv) (put-bytevector p bv)) bvs))))
  (define (parse-data x)
    (let-values (((id x) (parse-id x)))
      (match x
        ((('memory (? id-or-idx? mem)) offset . init)
         ;; Active data segment with explicit memory.
         (make-data id 'active mem (parse-offset offset)
                    (bytevector-concatenate init)))
        (((and offset (or ('offset . _) ('i32.const _))) . init)
         ;; Active data segment for memory 0.
         (make-data id 'active 0 (parse-offset offset)
                    (bytevector-concatenate init)))
        (init
         ;; Passive data segment.
         (make-data id 'passive #f #f (bytevector-concatenate init))))))
  (define (parse-tag tag)
    (let*-values (((id tag) (parse-id tag))
                  ((type tag) (parse-type-use tag)))
      (match tag
        (() (make-tag id type))
        (_ (error "bad tag" tag)))))
  (define (parse-start start)
    (let-values (((idx x) (parse-id-or-idx start)))
      (match x
        (() (or idx 0))
        (_ (error "bad start" x)))))

  (match (partition-clauses expr)
    (($ <wasm> id types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (let ((types (map parse-type-def types))
           (imports (map parse-import imports))
           (exports (map parse-export exports))
           (elems (map parse-elem elems))
           (datas (map parse-data datas))
           (tags (map parse-tag tags))
           (start (and start (parse-start start))))
       (define-syntax-rule (push! id val)
         (set! id (append! id (list val))))
       (define (visit-func x)
         (let*-values (((id x) (parse-id x))
                       ((id) (or id (fresh-id!))))
           (let lp ((x x))
             (match x
               ((('export name) . x)
                (push! exports (make-export name 'func id))
                (lp x))
               ((('import mod name) . x)
                (let-values (((type x) (parse-type-use x)))
                  (match x
                    (()
                     (push! imports (make-import mod name 'func id type))
                     #f)
                    (_
                     (error "bad import" x)))))
               (_
                (let-values (((type x) (parse-type-use x)))
                  (let lp ((x x) (locals '()))
                    (match x
                      ((('local (? id? id) vt) . x)
                       (lp x (cons (make-local id (parse-val-type vt)) locals)))
                      (_
                       (make-func id type (reverse locals)
                                  (parse-block x 'body)))))))))))
       (define (visit-table x)
         (let*-values (((id x) (parse-id x))
                       ((id) (or id (fresh-id!))))
           (let lp ((x x))
             (match x
               ((('export name) . x)
                (push! exports (make-export name 'table id))
                (lp x))
               ((('import mod name) . x)
                (let ((type (parse-table-type x)))
                  (push! imports (make-import mod name 'table id type))
                  #f))
               ((elemtype ... ('elem . segment))
                (let ((elemtype (parse-elem-type elemtype))
                      (len (length segment))
                      (offset '((i32.const 0))))
                  (push!
                   elems
                   (match segment
                     (((? id-or-idx? id) ...)
                      (make-elem #f 'active id 'funcref offset
                                 (map (lambda (id) `((ref.func ,id))) id)))
                     (((_ ...) ...)
                      (make-elem #f 'active id 'funcref offset
                                 (map (lambda (init)
                                        (parse-block init 'body))
                                      segment)))))
                  (make-table id
                              (make-table-type (make-limits len len)
                                               elemtype)
                              #f)))
               (_
                (make-table id (parse-table-type x) #f))))))
       (define (visit-memory x)
         (let*-values (((id x) (parse-id x))
                       ((id) (or id (fresh-id!))))
           (let lp ((x x))
             (match x
               ((('export name) . x)
                (push! exports (make-export name 'memory id))
                (lp x))
               ((('import mod name) . x)
                (let ((type (parse-mem-type x)))
                  (push! imports (make-import mod name 'memory id type))
                  #f))
               ((('data . data))
                (let* ((init (bytevector-concatenate data))
                       (len (bytevector-length init)))
                  (push! data (make-data id 'active 0 '((i32.const 0)) init))
                  (make-memory id (make-mem-type (make-limits len len)))))
               (_
                (make-memory id (parse-mem-type x)))))))
       (define (visit-global x)
         (let*-values (((id x) (parse-id x))
                       ((id) (or id (fresh-id!))))
           (let lp ((x x))
             (match x
               ((('export name) . x)
                (push! exports (make-export name 'global id))
                (lp x))
               ((('import mod name) type)
                (let ((type (parse-global-type type)))
                  (push! imports (make-import mod name 'global id type))
                  #f))
               ((type . init)
                (make-global id
                             (parse-global-type type)
                             (parse-block init 'body)))))))
       (let ((funcs (filter-map visit-func funcs))
             (tables (filter-map visit-table tables))
             (memories (filter-map visit-memory memories))
             (globals (filter-map visit-global globals)))
         (make-wasm id types imports funcs tables memories globals exports
                    start elems datas tags strings custom))))))

(define (wasm->wat mod)
  (match mod
    (($ <wasm> id types imports funcs tables memories globals exports start
               elems datas tags strings custom)
     ;; TODO: Factorize type-repr code that is duplicated between here
     ;; and (wasm dump).
     (define (val-type-repr vt)
       (match vt
         (($ <ref-type> #t ht)
          `(ref null ,ht))
         (($ <ref-type> #f ht)
          `(ref ,ht))
         (_ vt)))
     (define (params-repr params)
       (match params
         (() '())
         ((($ <param> #f type) ...)
          `((param ,@(map val-type-repr type))))
         ((($ <param> id type) . params)
          (cons `(param ,id ,(val-type-repr type))
                (params-repr params)))))
     (define (results-repr results)
       (map (lambda (type) `(result ,(val-type-repr type))) results))
     (define (field-repr field)
       (define (wrap mutable? repr)
         (if mutable? `(mut ,repr) repr))
       (match field
         (($ <field> id mutable? type)
          (let ((repr (wrap mutable? (val-type-repr type))))
            (if id
                `(field ,id ,repr)
                repr)))))
     (define (type-repr type)
       (match type
         (($ <func-sig> params results)
          `(func ,@(params-repr params) ,@(results-repr results)))
         (($ <sub-type> final? supers type)
          `(sub ,@(if final? '(final) '()) ,@supers ,(type-repr type)))
         (($ <struct-type> fields)
          `(struct ,@(map field-repr fields)))
         (($ <array-type> mutable? type)
          `(array ,(field-repr (make-field #f mutable? type))))))
     (define (type-use-repr type-use)
       (match type-use
         (($ <type-use> _ ($ <func-sig> params results))
          (append (params-repr params) (results-repr results)))))
     (define (block-type-repr bt)
       (match bt
         (#f '())
         ((? type-use? use)
          (type-use-repr use))
         ((? ref-type? rt)
          `((param ,(val-type-repr rt))))))
     (define (instr-repr instr)
       (define (make-prefix-arg prefix x)
         (if (zero? x)
             '()
             (list (string->symbol (string-append prefix (number->string x))))))
       (match instr
         (($ <mem-arg> id offset align)
          `(,id
            ,@(make-prefix-arg "offset=" offset)
            ,@(make-prefix-arg "align=" align)))
         (($ <ref-type> _ (? symbol? ht))
          `(,ht))
         (($ <type-use> idx)
          `((type ,idx)))
         ;; Instructions that need special handling:
         (('ref.cast ($ <ref-type> null? ht))
          `(ref.cast ,@(if null? '(null) '()) ,ht))
         (('string.const (? number? idx))
          `(string.const ,(list-ref strings idx)))
         (('if label bt consequent alternate)
          `(if ,@(block-type-repr bt)
               ,@(if label `(,label) '())
               (then ,@(instrs-repr consequent))
               (else ,@(instrs-repr alternate))))
         (((and op (or 'block 'loop)) label bt body)
          `(,op ,@(if label `(,label) '())
                ,@(block-type-repr bt)
                ,@(instrs-repr body)))
         ((_ ...)
          (append-map instr-repr instr))
         (_ `(,instr))))
     (define (instrs-repr instrs)
       (map instr-repr instrs))
     (define (limits-repr limits)
       (match limits
         (($ <limits> min max)
          (if max (list min max) (list min)))))
     (define (elem-item-repr init)
       `(item ,@(instrs-repr init)))
     `(module
       ,@(if id `(,id) '())
       ;; Types
       ,@(map (match-lambda
                (($ <type> id val)
                 `(type ,id ,(type-repr val)))
                (($ <rec-group> types)
                 `(rec ,@(map (match-lambda
                               (($ <type> id val)
                                `(type ,id ,(type-repr val))))
                              types))))
              types)
       ;; Imports
       ,@(map (match-lambda
                (($ <import> mod name 'func id type)
                 `(import ,mod ,name (func ,id ,@(type-use-repr type))))
                (($ <import> mod name 'global id ($ <global-type> mutable? type))
                 `(import ,mod ,name
                          (global ,id ,(if mutable?
                                           `(mut ,(val-type-repr type))
                                           (val-type-repr type)))))
                (($ <import> mod name 'memory id ($ <mem-type> limits))
                 `(import ,mod ,name (memory ,id ,@(limits-repr limits))))
                (($ <import> mod name 'table id ($ <table-type> limits elem-type))
                 `(import ,mod ,name
                          (table ,id ,@(limits-repr limits)
                                 ,(val-type-repr elem-type)))))
              imports)
       ;; Exports
       ,@(map (match-lambda
                (($ <export> name kind idx)
                 `(export ,name (,kind ,idx))))
              exports)
       ;; Globals
       ,@(map (match-lambda
                (($ <global> id ($ <global-type> mutable? type) init)
                 `(global ,id
                          ,(if mutable?
                               `(mut ,(val-type-repr type))
                               (val-type-repr type))
                          ,@(instrs-repr init))))
              globals)
       ;; Tables
       ,@(map (match-lambda
                (($ <table> id ($ <table-type> limits elem-type))
                 `(table ,id ,@(limits-repr limits)
                         ,(val-type-repr elem-type))))
              tables)
       ;; Memories
       ,@(map (match-lambda
                (($ <memory> id ($ <mem-type> limits))
                 `(memory ,id ,@(limits-repr limits))))
              memories)
       ;; Element segments
       ,@(map (match-lambda
                (($ <elem> id mode table type offset items)
                 (match mode
                   ('passive
                    `(elem ,id ,(val-type-repr type)
                           ,@(map elem-item-repr items)))
                   ('active
                    `(elem ,id (table ,table)
                           (offset ,@(instrs-repr offset))
                           ,(val-type-repr type)
                           ,@(map elem-item-repr items)))
                   ('declarative
                    `(elem ,id declare ,@(map elem-item-repr items))))))
              elems)
       ;; Data segments
       ,@(map (match-lambda
                (($ <data> id mode mem offset init)
                 (case mode
                   ((active)
                    `(data ,id ,mem ,@(instrs-repr offset) ,init))
                   ((passive)
                    `(data ,id ,init)))))
              datas)
       ;; Functions
       ,@(map (match-lambda
                (($ <func> id type locals body)
                 (match type
                   (($ <type-use> idx sig)
                    `(func ,(or id idx)
                           ,@(match (type-repr sig)
                               (('func . params+results)
                                params+results))
                           ,@(map (match-lambda
                                    (($ <local> id type)
                                     `(local ,id ,(val-type-repr type))))
                                  locals)
                           ,@(instrs-repr body))))))
              funcs)
       ;; Start function
       ,@(if start `((start ,start)) '())))))
