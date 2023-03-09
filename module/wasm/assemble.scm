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

(define-module (wasm assemble)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module ((srfi srfi-1) #:select (filter-map))
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-11)
  #:use-module (wasm types)
  #:export (parse-wat
            resolve-wasm
            assemble-wasm
            wat->wasm))

;; to-do:
;;  - support reftypes
;;  - support bulk memory instructions
;;  - tail calls
;;  - stringref

;; differences from standard: scheme comments / no block comments.
;; strings have guile string syntax; bytevectors also for data.  could
;; write standard-compliant parser instead (port from wassemble).

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

  (define (parse-ref-type x)
    (match x
      ((or 'funcref 'externref ('ref _)) x)
      (_ (error "bad reftype" x))))
  (define (parse-val-type x)
    (match x
      ((or 'i32 'i64 'f32 'f64) x)
      ((or 'funcref 'externref ('ref _)) x)
      (_ (error "bad valtype" x))))
  (define (parse-params x)
    (match x
      (((? id? id) vt) (list (make-param id vt)))
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
        (('func . sig) (parse-func-sig x))
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
  (define (parse-mem-arg x)
    (define (symbol-with-prefix prefix)
      (lambda (x)
        (string-prefix? prefix (symbol->string x))))
    (define (symbol-suffix x prefix)
      (substring (symbol->string x) (string-length prefix)))
    (define (parse-arg prefix x)
      (match x
        (((? (symbol-with-prefix prefix) arg) . x)
         (values
          (or (string->number (symbol-suffix arg prefix))
              (error "bad mem arg" arg)
              x)))
        (_ (values #f x))))
    (let*-values (((offset x) (parse-arg "offset=" x))
                  ((align x) (parse-arg "align=" x)))
      (values (make-mem-arg offset align) x)))
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
             then ,@consequent
             else ,@alternate
             end))
         (match body
           ((test ... ('then consequent ...))
            (finish test consequent '()))
           ((test ... ('then consequent ...) ('else alternate ...))
            (finish test consequent alternate)))))
      (((and tag (or 'br 'br_if 'call 'local.get 'local.set 'local.tee
                     'global.get 'global.tee))
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
                     'i64.store32'))
        . args)
       (let-values (((mem-arg args) (parse-mem-arg args)))
         `(,@args ,tag ,@(unfold-mem-arg mem-arg))))
      (((and tag (or 'i32.const 'i64.const 'f32.const 'f64.const)) val . insts)
       `(,@insts ,tag ,val))
      ((tag . args)
       `(,@args (,tag)))))
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
           ((or 'br 'br_if 'call 'local.get 'local.set 'local.tee 'global.get
                'global.tee)
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
            (let-values (((mem-arg in) (parse-mem-arg in)))
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

(define (resolve-wasm mod)
  (define counts (make-hash-table))
  (define type-ids (make-hash-table))
  (define func-ids (make-hash-table))
  (define table-ids (make-hash-table))
  (define memory-ids (make-hash-table))
  (define global-ids (make-hash-table))
  (define elem-ids (make-hash-table))
  (define data-ids (make-hash-table))
  (define tag-ids (make-hash-table))
  (define (add-id! id kind)
    (let ((idx (hashq-ref counts kind 0)))
      (hashq-set! counts kind (1+ idx))
      (when id
        (match kind
          ('type (hashq-set! type-ids id idx))
          ('func (hashq-set! func-ids id idx))
          ('table (hashq-set! table-ids id idx))
          ('memory (hashq-set! memory-ids id idx))
          ('global (hashq-set! global-ids id idx))
          ('elem (hashq-set! elem-ids id idx))
          ('data (hashq-set! data-ids id idx))
          ('tag (hashq-set! tag-ids id idx))))
      idx))
  (define interned-strings (make-hash-table))
  (define interned-string-count 0)
  (define (intern-string string)
    (or (hash-ref interned-strings string)
        (let ((idx interned-string-count))
          (hash-set! interned-strings string idx)
          (set! interned-string-count (1+ idx))
          idx)))
  (match mod
    (($ <wasm> types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (for-each (match-lambda (($ <type> id type)
                              (add-id! id 'type)))
               types)
     (for-each (match-lambda (($ <import> mod name kind id type)
                              (add-id! id kind)))
               imports)
     (for-each (match-lambda (($ <func> id type locals body)
                              (add-id! id 'func)))
               funcs)
     (for-each (match-lambda (($ <table> id type init)
                              (add-id! id 'table)))
               tables)
     (for-each (match-lambda (($ <memory> id type)
                              (add-id! id 'memory)))
               memories)
     (for-each (match-lambda (($ <global> id type init)
                              (add-id! id 'global)))
               globals)
     (for-each (match-lambda (($ <elem> id mode table type offset init)
                              (add-id! id 'elem)))
               elems)
     (for-each (match-lambda (($ <data> id mode mem offset init)
                              (add-id! id 'data)))
               datas)
     (for-each (match-lambda (($ <tag> id type)
                              (add-id! id 'tag)))
               tags)
     (for-each intern-string strings)

     (define (resolver ids kind)
       (lambda (id)
         (cond
          ((exact-integer? id) id)
          ((hashq-ref ids id))
          (else (error "unbound identifier" kind id)))))
     (define resolve-type (resolver type-ids "type"))
     (define resolve-func (resolver func-ids "func"))
     (define resolve-table (resolver table-ids "table"))
     (define resolve-memory (resolver memory-ids "memory"))
     (define resolve-global (resolver global-ids "global"))
     (define resolve-elem (resolver elem-ids "elem"))
     (define resolve-data (resolver data-ids "data"))
     (define resolve-tag (resolver tag-ids "tag"))

     (define (resolve-type-use x)
       (define (intern-func-sig/idx! params results idx)
         (set! types
               (cons (make-type #f (make-func-sig params results)) types)))
       (define (intern-func-sig! params results)
         (let lp ((types types) (idx 0))
           (define param-type (match-lambda (($ <param> id type) type)))
           (match types
             (()
              (let ((idx (add-id! #f 'type)))
                (intern-func-sig/idx! params results idx)
                idx))
             ((($ <type> id sig) . types)
              (match sig
                (($ <func-sig> params' results')
                 (if (and (equal? (map param-type params)
                                  (map param-type params'))
                          (equal? results results'))
                     idx
                     (lp types (1+ idx))))
                (_ (lp types (1+ idx))))))))
       (match x
         (($ <type-use> idx (and use-sig ($ <func-sig> params results)))
          (if idx
              (let ((idx (resolve-type idx)))
                (match (list-ref types idx)
                  (($ <type> _ def-sig)
                   (make-type-use idx
                                  (if (and (null? params) (null? results))
                                      def-sig
                                      use-sig)))))
              (make-type-use (intern-func-sig! params results)
                             use-sig)))))
     (define (resolve-block-type x)
       (match x
         (($ <type-use> #f ($ <func-sig> () (or () (_))))
          x)
         (_ (resolve-type-use x))))
     (define (resolve-instructions insts locals labels)
       (define (resolve-label label)
         (match label
           ((? exact-integer?) label)
           (_
            (or (list-index labels label)
                (error "unbound label" label labels)))))
       (define (resolve-local id)
         (match id
           ((? exact-integer?) id)
           (_
            (or (list-index locals id)
                (error "unbound local" id locals)))))
       (map
        (match-lambda
         (((and inst (or 'block 'loop)) label type body)
          (let ((labels (cons label labels)))
            `(,inst ,label ,(resolve-block-type type)
                    ,(resolve-instructions body locals labels))))
         (('if label type consequent alternate)
          (let ((labels (cons label labels)))
            `(if ,label ,(resolve-block-type type)
                 ,(resolve-instructions consequent locals labels)
                 ,(resolve-instructions alternate locals labels))))
         (((and inst (or 'br 'br_if)) label)
          `(,inst ,(resolve-label label)))
         (('br_table targets default)
          `(br_table ,(map resolve-label targets) ,(resolve-label default)))
         (('call label)
          `(call ,(resolve-func label)))
         (('call_indirect table type)
          `(call_indirect ,(resolve-table table) ,(resolve-type-use type)))
         (((and inst (or 'local.get 'local.set 'local.tee)) local)
          `(,inst ,(resolve-local local)))
         (((and inst (or 'global.get 'global.set)) global)
          `(,inst ,(resolve-global)))
         (('string.const (? string? str))
          `(string.const ,(intern-string str)))
         ;; fixme: tables, bulk memory, reftypes...
         (inst inst))
        insts))

     (define (visit-import import)
       (match import
         (($ <import> mod name 'func id type)
          (make-import mod name 'func id (resolve-type-use type)))
         (_ import)))

     (define (visit-export export)
       (match export
         (($ <export> name 'func id)
          (make-export name 'func (resolve-func id)))
         (($ <export> name 'table id)
          (make-export name 'table (resolve-table id)))
         (($ <export> name 'memory id)
          (make-export name 'memory (resolve-memory id)))
         (($ <export> name 'global id)
          (make-export name 'global (resolve-global id)))))

     (define (visit-elem elem)
       (match elem
         (($ <elem> id mode table type offset init)
          (make-elem id mode (and table (resolve-table table)) type
                     (resolve-instructions offset '() '())
                     (map (lambda (init)
                            (resolve-instructions init '() '()))
                          init)))))

     (define (visit-data data)
       (match data
         (($ <data> id mode mem offset init)
          (make-data id mode (and mem (resolve-memory mem))
                     (resolve-instructions offset '() '())
                     init))))

     (define (visit-start start)
       (and start (resolve-func start)))

     (define (visit-func func)
       (match func
         (($ <func> id type locals body)
          (make-func id (resolve-type-use type) locals
                     (resolve-instructions body locals '())))))

     (define (visit-table table)
       (match table
         (($ <table> id type init)
          ;; FIXME: resolve (ref $foo)
          (make-table id type (and init (resolve-instructions init '() '()))))))

     (define (visit-memory mem) mem)

     (define (visit-global global)
       (match global
         (($ <global> id type init)
          ;; fixme: resolve type
          (make-global id type (resolve-instructions init '() '())))))

     (define (visit-type type)
       ;; for now no reftypes
       type)

     (define (visit-tag tag)
       (match tag
         (($ <tag> id type)
          (make-tag id (resolve-type-use type)))))

     ;; FIXME: reftypes means we have to resolve types in more places

     ;; Resolving any inline (param ...) (result ...) type definition
     ;; may cause a new type to be added.  For that reason, put off
     ;; resolving types until after resolving other productions.
     (let ((imports (map visit-import imports))
           (exports (map visit-export exports))
           (elems (map visit-elem elems))
           (datas (map visit-data datas))
           (start (visit-start start))
           (funcs (map visit-func funcs))
           (tables (map visit-table tables))
           (memories (map visit-memory memories))
           (globals (map visit-global globals))
           (tags (map visit-tag tags))
           (strings (map car
                         (sort (hash-map->list cons interned-strings)
                               (match-lambda*
                                (((s1 . idx1) (s2 . idx2)) (< idx1 idx2)))))))
       (make-wasm (map visit-type types)
                  imports funcs tables memories globals exports start
                  elems datas tags strings custom)))))
  
(define (assemble-wasm wasm)
  (define (put-uleb port val)
    (let lp ((val val))
      (let ((next (ash val -7)))
        (if (zero? next)
            (put-u8 port val)
            (begin
              (put-u8 port (logior #x80 (logand val #x7f)))
              (lp next))))))

  (define (put-sleb port val)
    (let lp ((val val))
      (if (<= 0 (+ val 64) 127)
          (put-u8 port (logand val #x7f))
          (begin
            (put-u8 port (logior #x80 (logand val #x7f)))
            (lp (ash val -7))))))

  (define (put-u32le port val)
    (let ((bv (u32vector 0)))
      (bytevector-u32-set! bv 0 val (endianness little))
      (put-bytevector port bv)))

  (define (emit-u8 port val) (put-u8 port val))
  (define (emit-u32 port val) (put-uleb port val))
  (define (emit-s32 port val) (put-sleb port val))
  (define (emit-s64 port val) (put-sleb port val))
  (define (emit-f32 port val) (put-bytevector port (f32vector val)))
  (define (emit-f64 port val) (put-bytevector port (f64vector val)))

  (define (emit-vec port items emit)
    (emit-u32 port (length items))
    (for-each (lambda (item) (emit port item)) items))
  
  (define (emit-vec/u8 port bv)
    (emit-u32 port (bytevector-length bv))
    (put-bytevector port bv))
  
  (define (emit-heap-type port ht)
    (match ht
      ((and (? exact-integer?) (not (? negative?))) (put-sleb port ht))
      ('func (emit-u8 port #x70))
      ('extern (emit-u8 port #x6F))
      ('any (emit-u8 port #x6E))
      ('eq (emit-u8 port #x6D))
      ('i31 (emit-u8 port #x6A))
      ('noextern (emit-u8 port #x69))
      ('nofunc (emit-u8 port #x68))
      ('struct (emit-u8 port #x67))
      ('array (emit-u8 port #x66))
      ('none (emit-u8 port #x65))
      ('string (emit-u8 port #x64))
      ('stringview_wtf8 (emit-u8 port #x63))
      ('stringview_wtf16 (emit-u8 port #x62))
      ('stringview_iter (emit-u8 port #x61))

      (_ (error "unexpected heap type" ht))))

  (define (emit-val-type port vt)
    (match vt
      ('i32 (emit-u8 port #x7F))
      ('i64 (emit-u8 port #x7E))
      ('f32 (emit-u8 port #x7D))
      ('f64 (emit-u8 port #x7C))
      ('v128 (emit-u8 port #x7B))
      ('funcref (emit-u8 port #x70))
      ('externref (emit-u8 port #x6F))
      ('anyref (emit-u8 port #x6E))
      ('eqref (emit-u8 port #x6D))
      ('i31ref (emit-u8 port #x6A))
      ('nullexternref (emit-u8 port #x69))
      ('nullfuncref (emit-u8 port #x68))
      ('structref (emit-u8 port #x67))
      ('arrayref (emit-u8 port #x66))
      ('nullref (emit-u8 port #x65))
      ('stringref (emit-u8 port #x64))
      ('stringview_wtf8ref (emit-u8 port #x63))
      ('stringview_wtf16ref (emit-u8 port #x62))
      ('stringview_iterref (emit-u8 port #x61))

      (($ <ref-type> nullable? ht)
       (emit-u8 port (if nullable? #x6C #x6B))
       (emit-heap-type port ht))

      (_ (error "unexpected valtype" vt))))

  (define (emit-result-type port rt)
    (emit-vec port rt emit-val-type))

  (define (emit-block-type port bt)
    (match bt
      (#f (emit-u8 port #x40))
      ((or (? symbol?) ($ <ref-type>)) (emit-val-type port bt))
      (($ <type-use> #f ($ <func-sig> () ())) (emit-u8 port #x40))
      (($ <type-use> #f ($ <func-sig> () (vt))) (emit-val-type port vt))
      (($ <type-use> idx) (emit-u32 port idx))))

  (define (emit-limits port limits)
    (match limits
      (($ <limits> min #f)
       (emit-u8 port #x00)
       (emit-u32 port min))
      (($ <limits> min max)
       (emit-u8 port #x01)
       (emit-u32 port min)
       (emit-u32 port max))))

  (define (emit-ref-type port rt)
    (match rt
      ((or 'i32 'i64 'f32 'f64 'i128)
       (error "unexpected reftype" rt))
      (_ (emit-val-type port rt))))

  (define (emit-elem-type port et)
    (emit-ref-type port et))

  (define (emit-table-type port tt)
    (match tt
      (($ <table-type> limits elem-type)
       (emit-elem-type port elem-type)
       (emit-limits port limits))))

  (define (emit-mem-type port mt)
    (match mt
      (($ <mem-type> limits) (emit-limits port limits))))

  (define (emit-global-type port gt)
    (match gt
      (($ <global-type> mutable? vt)
       (emit-val-type port vt)
       (emit-u8 port (if mutable? 1 0)))))

  (define (emit-name port str)
    (emit-vec/u8 port (string->utf8 str)))

  (define (emit-end port)
    (emit-u8 port #x0B))

  (define (emit-instruction port inst)
    (define (bad-instruction) (error "bad instruction" inst))
                  
    (define-values (op args)
      (match inst ((op args ...) (values op args))))

    (define (emit code)
      (match args
        (() (emit-u8 port code))
        (_ (bad-instruction))))
    (define (emit-block code)
      (match args
        ((bt insts)
         (emit-u8 port code)
         (emit-block-type port bt)
         (emit-instructions port insts)
         (emit-end port))
        (_ (bad-instruction))))
    (define (emit-if code)
      (define else-code #x05)
      (match args
        ((bt consequent alternate)
         (emit-u8 port code)
         (emit-block-type port bt)
         (emit-instructions port consequent)
         (unless (null? alternate)
           (emit-u8 port else-code)
           (emit-instructions port alternate))
         (emit-end port))
        (_ (bad-instruction))))
    (define (emit-try code)
      (define catch-code #x07)
      (define delegate-code #x18)
      (define catch_all-code #x19)
      (match args
        ((label type body catches catch-all)
         (emit-u8 port code)
         (emit-instructions port body)
         (for-each (lambda (catch)
                     (emit-u8 port catch-code)
                     (emit-instructions port catch))
                   catches)
         (when catch-all
           (emit-u8 port catch_all-code)
           (emit-instructions port catch-all))
         (emit-end port))))
    (define (emit-try_delegate code)
      (define delegate-code #x18)
      (match args
        ((label type body delegate)
         (emit-u8 port code)
         (emit-instructions port body)
         (emit-u8 port delegate-code)
         (emit-u32 port delegate))))
    (define (emit-idx code)
      (match args
        ((idx)
         (emit-u8 port code)
         (emit-u32 port idx))
        (_ (bad-instruction))))
    (define (emit-br_table code)
      (match args
        ((targets default)
         (emit-u8 port code)
         (emit-vec port targets emit-u32)
         (emit-u32 port default))
        (_ (bad-instruction))))
    (define (emit-call_indirect code)
      (match args
        ((table type)
         (emit-u8 port code)
         (emit-u32 port type)
         (emit-u32 port table))
        (_ (bad-instruction))))
    (define (emit-select old-code new-code)
      (match args
        (()
         (emit-u8 port old-code))
        ((types)
         (emit-u8 port new-code)
         (emit-vec port types emit-val-type))
        (_ (bad-instruction))))
    (define (emit-mem code)
      (match args
        ((($ <mem-arg> offset align))
         (emit-u8 port code)
         (emit-u32 port align)
         (emit-u32 port offset))
        (_ (bad-instruction))))
    (define (emit-const code emit-val)
      (match args
        ((val)
         (emit-u8 port code)
         (emit-val port val))
        (_ (bad-instruction))))
    (define (emit-ht code)
      (match args
        ((ht)
         (emit-u8 port code)
         (emit-heap-type port ht))
        (_ (bad-instruction))))
    (define (emit-gc-op code)
      (emit-u8 port #xfb)
      (put-uleb port code))
    (define (emit-gc code)
      (match args
        (() (emit-gc-op code))
        (_ (bad-instruction))))
    (define (emit-gc-idx code)
      (match args
        ((idx)
         (emit-gc-op code)
         (emit-u32 port idx))
        (_ (bad-instruction))))
    (define (emit-gc-idx-idx code)
      (match args
        ((idx0 idx1)
         (emit-gc-op code)
         (emit-u32 port idx0)
         (emit-u32 port idx1))
        (_ (bad-instruction))))
    (define (emit-gc-idx-len code)
      (emit-gc-idx-idx code))
    (define (emit-gc-nullable-ht code nullable-code)
      (match args
        ((nullable? ht)
         (emit-gc-op (if nullable? nullable-code code))
         (emit-heap-type port ht))
        (_ (bad-instruction))))

    (match op
      ('unreachable         (emit #x00))
      ('nop                 (emit #x01))
      ('block               (emit-block #x02))
      ('loop                (emit-block #x03))
      ('if                  (emit-if #x04))
      ('try                 (emit-try #x06))
      ('try_delegate        (emit-try_delegate #x06))
      ('throw               (emit-idx #x08))
      ('rethrow             (emit-idx #x09))
      ('br                  (emit-idx #x0C))
      ('br_if               (emit-idx #x0D))
      ('br_table            (emit-br_table #x0E))
      ('return              (emit #x0F))
      ('call                (emit-idx #x10))
      ('call_indirect       (emit-call_indirect #x11))
      ('return_call         (emit-idx #x12))
      ('return_call_indirect (emit-call_indirect #x13))
      ('call_ref            (emit-idx #x14))
      ('return_call_ref     (emit-idx #x15))
      ('drop                (emit #x1A))
      ('select              (emit-select #x1B #x1C))
      ('local.get           (emit-idx #x20))
      ('local.set           (emit-idx #x21))
      ('local.tee           (emit-idx #x22))
      ('global.get          (emit-idx #x23))
      ('global.set          (emit-idx #x24))
      ('i32.load            (emit-mem #x28))
      ('i64.load            (emit-mem #x29))
      ('f32.load            (emit-mem #x2A))
      ('f64.load            (emit-mem #x2B))
      ('i32.load8_s         (emit-mem #x2C))
      ('i32.load8_u         (emit-mem #x2D))
      ('i32.load16_s        (emit-mem #x2E))
      ('i32.load16_u        (emit-mem #x2F))
      ('i64.load8_s         (emit-mem #x30))
      ('i64.load8_u         (emit-mem #x31))
      ('i64.load16_s        (emit-mem #x32))
      ('i64.load16_u        (emit-mem #x33))
      ('i64.load32_s        (emit-mem #x34))
      ('i64.load32_u        (emit-mem #x35))
      ('i32.store           (emit-mem #x36))
      ('i64.store           (emit-mem #x37))
      ('f32.store           (emit-mem #x38))
      ('f64.store           (emit-mem #x39))
      ('i32.store8          (emit-mem #x3A))
      ('i32.store16         (emit-mem #x3B))
      ('i64.store8          (emit-mem #x3C))
      ('i64.store16         (emit-mem #x3D))
      ('i64.store32         (emit-mem #x3E))
      ('memory.size         (emit-idx #x3F))
      ('memory.grow         (emit-idx #x40))
      ('i32.const           (emit-const #x41 emit-s32))
      ('i64.const           (emit-const #x42 emit-s64))
      ('f32.const           (emit-const #x43 emit-f32))
      ('f64.const           (emit-const #x44 emit-f64))
      ('i32.eqz             (emit #x45))
      ('i32.eq              (emit #x46))
      ('i32.ne              (emit #x47))
      ('i32.lt_s            (emit #x48))
      ('i32.lt_u            (emit #x49))
      ('i32.gt_s            (emit #x4A))
      ('i32.gt_u            (emit #x4B))
      ('i32.le_s            (emit #x4C))
      ('i32.le_u            (emit #x4D))
      ('i32.ge_s            (emit #x4E))
      ('i32.ge_u            (emit #x4F))
      ('i64.eqz             (emit #x50))
      ('i64.eq              (emit #x51))
      ('i64.ne              (emit #x52))
      ('i64.lt_s            (emit #x53))
      ('i64.lt_u            (emit #x54))
      ('i64.gt_s            (emit #x55))
      ('i64.gt_u            (emit #x56))
      ('i64.le_s            (emit #x57))
      ('i64.le_u            (emit #x58))
      ('i64.ge_s            (emit #x59))
      ('i64.ge_u            (emit #x5A))
      ('f32.eq              (emit #x5B))
      ('f32.ne              (emit #x5C))
      ('f32.lt              (emit #x5D))
      ('f32.gt              (emit #x5E))
      ('f32.le              (emit #x5F))
      ('f32.ge              (emit #x60))
      ('f64.eq              (emit #x61))
      ('f64.ne              (emit #x62))
      ('f64.lt              (emit #x63))
      ('f64.gt              (emit #x64))
      ('f64.le              (emit #x65))
      ('f64.ge              (emit #x66))
      ('i32.clz             (emit #x67))
      ('i32.ctz             (emit #x68))
      ('i32.popcnt          (emit #x69))
      ('i32.add             (emit #x6A))
      ('i32.sub             (emit #x6B))
      ('i32.mul             (emit #x6C))
      ('i32.div_s           (emit #x6D))
      ('i32.div_u           (emit #x6E))
      ('i32.rem_s           (emit #x6F))
      ('i32.rem_u           (emit #x70))
      ('i32.and             (emit #x71))
      ('i32.or              (emit #x72))
      ('i32.xor             (emit #x73))
      ('i32.shl             (emit #x74))
      ('i32.shr_s           (emit #x75))
      ('i32.shr_u           (emit #x76))
      ('i32.rotl            (emit #x77))
      ('i32.rotr            (emit #x78))
      ('i64.clz             (emit #x79))
      ('i64.ctz             (emit #x7A))
      ('i64.popcnt          (emit #x7B))
      ('i64.add             (emit #x7C))
      ('i64.sub             (emit #x7D))
      ('i64.mul             (emit #x7E))
      ('i64.div_s           (emit #x7F))
      ('i64.div_u           (emit #x80))
      ('i64.rem_s           (emit #x81))
      ('i64.rem_u           (emit #x82))
      ('i64.and             (emit #x83))
      ('i64.or              (emit #x84))
      ('i64.xor             (emit #x85))
      ('i64.shl             (emit #x86))
      ('i64.shr_s           (emit #x87))
      ('i64.shr_u           (emit #x88))
      ('i64.rotl            (emit #x89))
      ('i64.rotr            (emit #x8A))
      ('f32.abs             (emit #x8B))
      ('f32.neg             (emit #x8C))
      ('f32.ceil            (emit #x8D))
      ('f32.floor           (emit #x8E))
      ('f32.trunc           (emit #x8F))
      ('f32.nearest         (emit #x90))
      ('f32.sqrt            (emit #x91))
      ('f32.add             (emit #x92))
      ('f32.sub             (emit #x93))
      ('f32.mul             (emit #x94))
      ('f32.div             (emit #x95))
      ('f32.min             (emit #x96))
      ('f32.max             (emit #x97))
      ('f32.copysign        (emit #x98))
      ('f64.abs             (emit #x99))
      ('f64.neg             (emit #x9A))
      ('f64.ceil            (emit #x9B))
      ('f64.floor           (emit #x9C))
      ('f64.trunc           (emit #x9D))
      ('f64.nearest         (emit #x9E))
      ('f64.sqrt            (emit #x9F))
      ('f64.add             (emit #xA0))
      ('f64.sub             (emit #xA1))
      ('f64.mul             (emit #xA2))
      ('f64.div             (emit #xA3))
      ('f64.min             (emit #xA4))
      ('f64.max             (emit #xA5))
      ('f64.copysign        (emit #xA6))
      ('i32.wrap_i64        (emit #xA7))
      ('i32.trunc_f32_s     (emit #xA8))
      ('i32.trunc_f32_u     (emit #xA9))
      ('i32.trunc_f64_s     (emit #xAA))
      ('i32.trunc_f64_u     (emit #xAB))
      ('i64.extend_i32_s    (emit #xAC))
      ('i64.extend_i32_u    (emit #xAD))
      ('i64.trunc_f32_s     (emit #xAE))
      ('i64.trunc_f32_u     (emit #xAF))
      ('i64.trunc_f64_s     (emit #xB0))
      ('i64.trunc_f64_u     (emit #xB1))
      ('f32.convert_i32_s   (emit #xB2))
      ('f32.convert_i32_u   (emit #xB3))
      ('f32.convert_i64_s   (emit #xB4))
      ('f32.convert_i64_u   (emit #xB5))
      ('f32.demote_f64      (emit #xB6))
      ('f64.convert_i32_s   (emit #xB7))
      ('f64.convert_i32_u   (emit #xB8))
      ('f64.convert_i64_s   (emit #xB9))
      ('f64.convert_i64_u   (emit #xBA))
      ('f64.promote_f32     (emit #xBB))
      ('i32.reinterpret_f32 (emit #xBC))
      ('i64.reinterpret_f64 (emit #xBD))
      ('f32.reinterpret_i32 (emit #xBE))
      ('f64.reinterpret_i64 (emit #xBF))
      ('i32.extend8_s       (emit #xc0))
      ('i32.extend16_s      (emit #xc1))
      ('i64.extend8_s       (emit #xc2))
      ('i64.extend16_s      (emit #xc3))
      ('i64.extend32_s      (emit #xc4))
      ('ref.null            (emit-ht #xd0))
      ('ref.is_null         (emit #xd1))
      ('ref.func            (emit-idx #xd2))
      ('ref.as_non_null     (emit #xd3))
      ('br_on_null          (emit-idx #xd4))
      ('ref.eq              (emit #xd5))
      ('br_on_non_null      (emit-idx #xd6))

      ;; GC opcodes
      ;; Note: these binary encodings are temporary; they will
      ;; change in fall 2023 when the GC proposal reaches the
      ;; standard.
      ('struct.get                         (emit-gc-idx-idx #x03))
      ('struct.get_s                       (emit-gc-idx-idx #x04))
      ('struct.get_u                       (emit-gc-idx-idx #x05))
      ('struct.set                         (emit-gc-idx-idx #x06))
      ('struct.new                         (emit-gc-idx #x07))
      ('struct.new_default                 (emit-gc-idx #x08))
      ('array.get                          (emit-gc-idx #x13))
      ('array.get_s                        (emit-gc-idx #x14))
      ('array.get_u                        (emit-gc-idx #x15))
      ('array.set                          (emit-gc-idx #x16))
      ('array.copy                         (emit-gc-idx-idx #x18))
      ('array.len                          (emit-gc #x19))
      ('array.new_fixed                    (emit-gc-idx-len #x1a))
      ('array.new                          (emit-gc-idx #x1b))
      ('array.new_default                  (emit-gc-idx #x1c))
      ('array.new_data                     (emit-gc-idx-idx #x1d))
      ('array.new_elem                     (emit-gc-idx-idx #x1f))
      ('i31.new                            (emit-gc #x20))
      ('i31.get_s                          (emit-gc #x21))
      ('i31.get_u                          (emit-gc #x22))
      ('ref.test                           (emit-gc-nullable-ht #x40 #x48))
      ('ref.cast                           (emit-gc-nullable-ht #x41 #x49))
      ('br_on_cast                         (emit-gc-nullable-ht #x42 #x4a))
      ('br_on_cast_fail                    (emit-gc-nullable-ht #x43 #x4b))
      ('extern.internalize                 (emit-gc #x70))
      ('extern.externalize                 (emit-gc #x71))

      ('string.new_utf8                    (emit-gc-idx #x80))
      ('string.new_wtf16                   (emit-gc-idx #x81))
      ('string.const                       (emit-gc-idx #x82))
      ('string.measure_utf8                (emit-gc #x83))
      ('string.measure_wtf8                (emit-gc #x84))
      ('string.measure_wtf16               (emit-gc #x85))
      ('string.encode_utf8                 (emit-gc-idx #x86))
      ('string.encode_wtf16                (emit-gc-idx #x87))
      ('string.concat                      (emit-gc #x88))
      ('string.eq                          (emit-gc #x89))
      ('string.is_usv_sequence             (emit-gc #x8a))
      ('string.new_lossy_utf8              (emit-gc-idx #x8b))
      ('string.new_wtf8                    (emit-gc-idx #x8c))
      ('string.encode_lossy_utf8           (emit-gc-idx #x8d))
      ('string.encode_wtf8                 (emit-gc-idx #x8e))
      ('string.as_wtf8                     (emit-gc #x90))
      ('stringview_wtf8.advance            (emit-gc #x91))
      ('stringview_wtf8.encode_utf8        (emit-gc-idx #x92))
      ('stringview_wtf8.slice              (emit-gc #x93))
      ('stringview_wtf8.encode_lossy_utf8  (emit-gc-idx #x94))
      ('stringview_wtf8.encode_wtf8        (emit-gc-idx #x95))
      ('string.as_wtf16                    (emit-gc #x98))
      ('stringview_wtf16.length            (emit-gc #x99))
      ('stringview_wtf16.get_codeunit      (emit-gc #x9a))
      ('stringview_wtf16.encode            (emit-gc-idx #x9b))
      ('stringview_wtf16.slice             (emit-gc #x9c))
      ('string.as_iter                     (emit-gc #xa0))
      ('stringview_iter.next               (emit-gc #xa1))
      ('stringview_iter.advance            (emit-gc #xa2))
      ('stringview_iter.rewind             (emit-gc #xa3))
      ('stringview_iter.slice              (emit-gc #xa4))
      ('string.compare                     (emit-gc #xa8))
      ('string.from_code_point             (emit-gc #xa9))
      ('string.new_utf8_array              (emit-gc #xb0))
      ('string.new_wtf16_array             (emit-gc #xb1))
      ('string.encode_utf8_array           (emit-gc #xb2))
      ('string.encode_wtf16_array          (emit-gc #xb3))
      ('string.new_lossy_utf8_array        (emit-gc #xb4))
      ('string.new_wtf8_array              (emit-gc #xb5))
      ('string.encode_lossy_utf8_array     (emit-gc #xb6))
      ('string.encode_wtf8_array           (emit-gc #xb7))

      (_ (bad-instruction))))
  
  (define (emit-instructions port insts)
    (for-each (lambda (inst) (emit-instruction port inst)) insts))

  (define (emit-expr port expr)
    (emit-instructions port expr)
    (emit-end port))

  (define (emit-type-def port def)
    (match def
      (($ <type> _ ($ <func-sig>
                      (($ <param> _ param-type) ...) (result-type ...)))
       (emit-u8 port #x60)
       (emit-result-type port param-type)
       (emit-result-type port result-type))))

  (define (emit-type-use port type)
    (match type
      (($ <type-use> idx)
       (emit-u32 port idx))))

  (define (emit-import port import)
    (match import
      (($ <import> mod name kind id type)
       (emit-name port mod)
       (emit-name port name)
       (match kind
         ('func
          (emit-u8 port #x00)
          (emit-type-use port type))
         ('table
          (emit-u8 port #x01)
          (emit-table-type port type))
         ('mem
          (emit-u8 port #x02)
          (emit-mem-type port type))
         ('global
          (emit-u8 port #x03)
          (emit-global-type port type))))))

  (define (emit-func-decl port func)
    (match func
      (($ <func> id type locals body)
       (emit-type-use port type))))

  (define (emit-table port table)
    (match table
      (($ <table> id type init)
       (emit-table-type port type))))

  (define (emit-memory port memory)
    (match memory
      (($ <memory> id type)
       (emit-mem-type port type))))

  (define (emit-global port global)
    (match global
      (($ <global> id type init)
       (emit-global-type port type)
       (emit-expr port init))))

  (define (emit-export port export)
    (match export
      (($ <export> name kind id)
       (emit-name port name)
       (match kind
         ('func (emit-u8 port #x00))
         ('table (emit-u8 port #x01))
         ('mem (emit-u8 port #x02))
         ('global (emit-u8 port #x03)))
       (emit-u32 port id))))

  (define (emit-element port elem)
    (match elem
      (($ <elem> id 'active 0 'funcref offset ((('ref.func idx)) ...))
       (emit-u8 port #x00)
       (emit-expr port offset)
       (emit-vec port idx emit-u32))
      (($ <elem> id 'passive #f 'funcref #f ((('ref.func idx)) ...))
       (emit-u8 port #x01)
       (emit-u8 port #x00) ;; elemkind: funcref
       (emit-vec port idx emit-u32))
      (($ <elem> id 'active table 'funcref offset ((('ref.func idx)) ...))
       (emit-u8 port #x02)
       (emit-u32 port table)
       (emit-expr port offset)
       (emit-u8 port #x00) ;; elemkind: funcref
       (emit-vec port idx emit-u32))
      (($ <elem> id 'declarative #f 'funcref #f ((('ref.func idx)) ...))
       (emit-u8 port #x03)
       (emit-u8 port #x00) ;; elemkind: funcref
       (emit-vec port idx emit-u32))
      (($ <elem> id 'active 0 'funcref offset (expr ...))
       (emit-u8 port #x04)
       (emit-expr port offset)
       (emit-vec port expr emit-expr))
      (($ <elem> id 'passive #f type #f (expr ...))
       (emit-u8 port #x05)
       (emit-ref-type port type)
       (emit-vec port expr emit-expr))
      (($ <elem> id 'active table type offset (expr ...))
       (emit-u8 port #x06)
       (emit-u32 port table)
       (emit-expr port offset)
       (emit-ref-type port type)
       (emit-vec port expr emit-expr))
      (($ <elem> id 'declarative #f type #f (expr ...))
       (emit-u8 port #x07)
       (emit-ref-type port type)
       (emit-vec port expr emit-expr))))

  (define (emit-func-def port func)
    (define (emit-compressed-locals port locals)
      (define compressed
        (let compress ((locals locals))
         (match locals
           (() '())
           ((($ <local> id type) . locals)
            (match (compress locals)
              (((count . (? (lambda (vt) (equal? vt type)))) . compressed)
               (acons (1+ count) type locals))
              (compressed (acons 1 type compressed)))))))
      (emit-vec port compressed
                (lambda (port pair)
                  (match pair
                    ((count . vt)
                     (emit-u32 port count)
                     (emit-val-type port vt))))))
    (match func
      (($ <func> id type locals body)
       (emit-vec/u8 port
                    (call-with-output-bytevector
                     (lambda (port)
                       (emit-compressed-locals port locals)
                       (emit-expr port body)))))))

  (define (emit-data port data)
    (match data
      (($ <data> id 'active 0 offset init)
       (emit-u8 port #x00)
       (emit-expr port offset)
       (emit-vec/u8 port init))
      (($ <data> id 'passive #f offset init)
       (emit-u8 port #x01)
       (emit-vec/u8 port init))
      (($ <data> id 'active mem offset init)
       (emit-u8 port #x02)
       (emit-u32 port mem)
       (emit-expr port offset)
       (emit-vec/u8 port init))))

  (define (emit-tag port tag)
    (match tag
      (($ <tag> id ($ <type-use> idx _))
       (emit-u32 port idx))))

  (define (emit-section port code bytes)
    (emit-u8 port code)
    (emit-vec/u8 port bytes))

  (define (emit-vec-section port code items emit-item)
    (unless (null? items)
      (emit-section port code
                    (call-with-output-bytevector
                     (lambda (port)
                       (emit-vec port items emit-item))))))
  
  (match wasm
    (($ <wasm> types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (call-with-output-bytevector
      (lambda (port)
        (put-bytevector port #vu8(#x00 #x61 #x73 #x6d)) ;; "\0asm"
        (put-bytevector port #vu8(1 0 0 0)) ;; version
        (emit-vec-section port 1 types emit-type-def)
        (emit-vec-section port 2 imports emit-import)
        (emit-vec-section port 3 funcs emit-func-decl)
        (emit-vec-section port 4 tables emit-table)
        (emit-vec-section port 5 memories emit-memory)
        (emit-vec-section port 13 tags emit-tag)
        (unless (null? strings)
          (emit-section port 14 (call-with-output-bytevector
                                 (lambda (port)
                                   (emit-u8 port #x00)
                                   (emit-vec port strings emit-name)))))
        (emit-vec-section port 6 globals emit-global)
        (emit-vec-section port 7 exports emit-export)
        (when start
          (emit-section port 8 (call-with-output-bytevector
                                (lambda (port)
                                  (emit-u32 port start)))))
        (emit-vec-section port 9 elems emit-export)
        (unless (null? datas)
          (emit-section port 12 (call-with-output-bytevector
                                 (lambda (port)
                                   (emit-u32 port (length datas))))))
        (emit-vec-section port 10 funcs emit-func-def)
        (emit-vec-section port 11 datas emit-data))))))

(define (wat->wasm expr)
  (assemble-wasm (resolve-wasm (parse-wat expr))))
