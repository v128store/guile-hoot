;;; WebAssembly assembler
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
;;; Assembler for WebAssembly.
;;;
;;; Code:

(define-module (wasm wat)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (filter-map))
  #:use-module (srfi srfi-11)
  #:use-module (wasm types)
  #:export (parse-wat))

;; to-do:
;;  - support reftypes
;;  - support bulk memory instructions
;;  - tail calls
;;  - stringref

;; differences from standard: scheme comments / no block comments.
;; strings have guile string syntax; bytevectors also for data.  could
;; write standard-compliant parser instead (port from wassemble).

(define (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define (natural-alignment inst)
  (case inst
    ((i32.load8_s
      i32.load8_u
      i64.load8_s
      i64.load8_u
      i32.store8
      i64.store8)
     8)
    ((i32.load16_s
      i32.load16_u
      i64.load16_s
      i64.load16_u
      i32.store16
      i64.store16)
     16)
    ((i32.load
      f32.load
      i64.load32_s
      i64.load32_u
      i32.store
      f32.store
      i64.store32)
     32)
    ((i64.load
      f64.load
      i64.store
      f64.store)
     64)))

(define (parse-wat expr)
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
        (_ (error "unexpected form in module" x))))
    (match x
      (('module . clauses) (for-each collect-raw clauses))
      ((clauses ...) (for-each collect-raw clauses))
      (_ (error "unexpected module" x)))
    (make-wasm (reverse types) (reverse imports) (reverse funcs)
               (reverse tables) (reverse memories) (reverse globals)
               (reverse exports) start (reverse elems)
               (reverse datas) (reverse tags) strings '()))

  (define (parse-heap-type x)
    (match x
      ((or 'func 'extern (? id-or-idx?)) x)
      (_ (error "bad heaptype" x))))
  (define (parse-ref-type x)
    (match x
      ((or 'funcref 'externref) x)
      (('ref 'null ht) (make-ref-type #t (parse-heap-type ht)))
      (('ref ht) (make-ref-type #f (parse-heap-type ht)))
      (_ (error "bad reftype" x))))
  (define (parse-val-type x)
    (match x
      ((or 'i32 'i64 'f32 'f64) x)
      ((or 'funcref 'externref) x)
      (('ref 'null ht) (make-ref-type #t (parse-heap-type ht)))
      (('ref ht) (make-ref-type #f (parse-heap-type ht)))
      (_ (error "bad valtype" x))))
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
    (parse-type-use x))
  (define (parse-limits x)
    (match x
      (((? u32? min) (? u32? max) . tail)
       (values (make-limits min max) tail))
      (((? u32? min) . tail)
       (values (make-limits min #f) tail))))
  (define (parse-elem-type x)
    (match x
      (('funcref) 'funcref)
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
    (error "unimplemented" x))
  (define (parse-struct-type x)
    (error "unimplemented" x))
  (define (parse-sub-type x)
    (error "unimplemented" x))
  (define (parse-type x)
    (define (parse-prim-type x)
      (match x
        (('func . sig) (parse-func-sig sig))
        (('array . sig) (parse-array-type sig))
        (('struct . sig) (parse-struct-type sig))))
    (match x
      (('sub (? id-or-idx? id) sub)
       (make-sub-type (list id) (parse-prim-type sub)))
      (_ (parse-prim-type x))))
  (define (parse-type-def x)
    (match x
      (((? id? id) t) (make-type id (parse-type t)))
      ((t) (make-type #f (parse-type t)))))
  (define (parse-import x)
    (define (parse-inner mod name kind id tail)
      (match kind
        ('func (make-import mod name 'func id (parse-type-use tail)))
        ('table (make-import mod name 'table id (parse-table-type tail)))
        ('memory (make-import mod name 'memory id (parse-mem-type tail)))
        ('global (make-import mod name 'global id (parse-global-type tail)))))
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
    (let*-values (((offset x) (parse-arg "offset=" x))
                  ((align x) (parse-arg "align=" x)))
      (values (make-mem-arg (or offset 0)
                            (or align (natural-alignment inst)))
              x)))
  (define (unfold-instruction inst)
    (define (unfold-type-use type)
      (match type
        (($ <type-use> #f ($ <func-sig> params results))
         `(,@(map (match-lambda
                   ((#f . vt) `(param ,vt))
                   ((id . vt) `(param ,id ,vt)))
                  params)
           (result ,@results)))
        (($ <type-use> idx _)
         `((type ,idx)))))
    (define (unfold-mem-arg arg)
      (match arg
        (($ <mem-arg> offset align)
         `(,@(if offset
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
      (('br_table . args)
       (let lp ((args args) (targets '()))
         (match args
           (((? id-or-idx? target) . args)
            (lp args (cons target targets)))
           (_ `(,@args br_table ,@(reverse targets))))))
      (('call_indirect . inst)
       (let-values (((type inst) (parse-type-use inst)))
         `(,@inst 'call_indirect ,@(unfold-type-use type))))
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
      (('ref.func id . args)
       `(,@args ref.func ,id))
      (((and tag (or 'call_ref 'return_call_ref)) (? id-or-idx? id) . args)
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
           ((or 'br 'br_if 'call 'local.get 'local.set 'local.tee 'global.get
                'global.set)
            (let-values (((idx in) (parse-id-or-idx in)))
              (unless idx (error "missing idx" inst in))
              (lp/inst in `(,inst ,idx))))
           ('br_table
            (let lp ((in in) (targets '()))
              (match in
                (((? id-or-idx? target) . in)
                 (lp in (cons target targets)))
                (_
                 (match targets
                   ((target ... default)
                    (lp/inst in `(,inst ,targets ,default)))
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
           (_
            (lp/inst in inst)))))))
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
    (($ <wasm> types imports funcs tables memories globals exports start
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
         (make-wasm types imports funcs tables memories globals exports start
                    elems datas tags strings custom))))))
