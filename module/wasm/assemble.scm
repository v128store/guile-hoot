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
  #:use-module ((srfi srfi-1) #:select (list-index))
  #:use-module (srfi srfi-4)
  #:use-module (srfi srfi-11)
  #:use-module (wasm wat)
  #:use-module (wasm types)
  #:export (resolve-wasm
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

(define (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define (resolve-wasm mod)
  (define (make-name-store)
    (let ((count 0)
          (ids (make-hash-table)))
      (values (lambda (id)
                (let ((idx count))
                  (set! count (1+ count))
                  (when id (hashq-set! ids id idx))
                  idx))
              (lambda (id)
                (cond
                 ((exact-integer? id) id)
                 ((hashq-ref ids id))
                 (else (error "unbound identifier" id)))))))
  (define-values (add-type-id! resolve-type) (make-name-store))
  (define-values (add-func-id! resolve-func) (make-name-store))
  (define-values (add-table-id! resolve-table) (make-name-store))
  (define-values (add-memory-id! resolve-memory) (make-name-store))
  (define-values (add-global-id! resolve-global) (make-name-store))
  (define-values (add-elem-id! resolve-elem) (make-name-store))
  (define-values (add-data-id! resolve-data) (make-name-store))
  (define-values (add-tag-id! resolve-tag) (make-name-store))
  (define struct-fields (make-hash-table))
  (define (add-struct-field! struct-id struct-idx field-id)
    (match (hashq-ref struct-fields struct-idx)
      (#f
       (let-values (((add-id! resolve-id) (make-name-store)))
         (let ((pair (cons add-id! resolve-id)))
           (hashq-set! struct-fields struct-idx pair)
           (when struct-id
             (hashq-set! struct-fields struct-id pair)))
         (add-id! field-id)))
      ((add-id! . resolve-id)
       (add-id! field-id))))
  (define (resolve-struct-field struct-id-or-idx field)
    (match (hashq-ref struct-fields struct-id-or-idx)
      ((add-id! . resolve-id)
       (resolve-id field))))
  (define interned-strings (make-hash-table))
  (define interned-string-count 0)
  (define (intern-string string)
    (or (hash-ref interned-strings string)
        (let ((idx interned-string-count))
          (hash-set! interned-strings string idx)
          (set! interned-string-count (1+ idx))
          idx)))
  (define (find-type pred types)
    (let lp ((types types) (idx 0))
      (define (visit-base rec supers type-id type-idx type)
        (pred rec type-id type-idx supers type))
      (define (visit-sub rec type-id type-idx type)
        (match type
          (($ <sub-type> supers type)
           (visit-base rec supers type-id type-idx type))
          (_ (visit-base rec '()  type-id type-idx type))))
      (match types
        (() #f)
        ((($ <rec-group> (subtypes)) . types)
         (let ((rec idx))
           (let lp ((subtypes subtypes) (idx idx))
             (match subtypes
               (() (lp types idx))
               ((($ <type> id subtype) . subtypes)
                (or (visit-sub rec id idx subtype)
                    (lp subtypes (1+ idx))))))))
        ((($ <type> id type) . types)
         (or (visit-sub idx id idx type)
             (lp types (1+ idx)))))))

  (define (type-use-matcher params results)
    (define param-type (match-lambda (($ <param> id type) type)))
    (lambda (rec type-id type-idx supers type)
      (and (null? supers)
           (match type
             (($ <func-sig> params' results')
              (and (equal? (map param-type params)
                           (map param-type params'))
                   (equal? results results')
                   (make-type-use type-idx type)))
             (_ #f)))))

  (define (adjoin-types-from-type-uses types funcs imports tags)
    (define (adjoin-type-use type types)
      (match type
        (($ <type-use> #f ($ <func-sig> params results))
         (if (find-type (type-use-matcher params results) types)
             types
             (append types
                     (list (make-type #f (make-func-sig params results))))))
        (($ <type-use>) types)))
    (define (adjoin-type-uses-from-import import types)
      (match import
        (($ <import> mod name 'func id type)
         (adjoin-type-use type types))
        (($ <import>) #f)))
    (define (adjoin-type-uses-from-tag tag types)
      (match tag
        (($ <tag> id type) (adjoin-type-use type types))))
    (define (adjoin-type-uses-from-func func types)
      (define (adjoin-type-use-for-block-type x types)
        (match x
          (($ <type-use> #f ($ <func-sig> () (or () (_))))
           types)
          (_ (adjoin-type-use x types))))
      (define (adjoin-type-uses-for-inst inst types)
        (match inst
          (((or 'block 'loop) label type body)
           (fold1 adjoin-type-uses-for-inst body
                  (adjoin-type-use-for-block-type type types)))
          (('if label type consequent alternate)
           (adjoin-type-uses-from-body
            consequent
            (adjoin-type-uses-from-body
             alternate
             (adjoin-type-use-for-block-type type types))))
          (('try label type body catches catch-all)
           (fold1 adjoin-type-uses-from-body (append body catches)
                  (adjoin-type-use-for-block-type
                   type
                   (if catch-all
                       (adjoin-type-uses-from-body catch-all types)
                       types))))
          (('try label type body handler)
           (adjoin-type-uses-from-body
            body
            (adjoin-type-use-for-block-type type types)))
          (((or 'call_indirect 'return_call_indirect) table type)
           (adjoin-type-use type types))
          (_ types)))
      (define (adjoin-type-uses-from-body insts types)
        (fold1 adjoin-type-uses-for-inst insts types))
      (match func
        (($ <func> id type locals body)
         (adjoin-type-uses-from-body body (adjoin-type-use type types)))))
    (fold1 adjoin-type-uses-from-func funcs
           (fold1 adjoin-type-uses-from-tag tags
                  (fold1 adjoin-type-uses-from-import imports types))))

  (match mod
    (($ <wasm> %types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (define types (adjoin-types-from-type-uses %types funcs imports tags))

     (for-each (match-lambda (($ <type> id type) (add-type-id! id))
                             (($ <rec-group> (($ <type> id type) ...))
                              (for-each add-type-id! id)))
               types)
     (for-each (match-lambda (($ <import> mod name kind id type)
                              (match kind
                                ('func (add-func-id! id))
                                ('global (add-global-id! id))
                                ('table (add-table-id! id))
                                ('memory (add-memory-id! id)))))
               imports)
     (for-each (match-lambda (($ <func> id type locals body)
                              (add-func-id! id)))
               funcs)
     (for-each (match-lambda (($ <table> id type init)
                              (add-table-id! id)))
               tables)
     (for-each (match-lambda (($ <memory> id type)
                              (add-memory-id! id)))
               memories)
     (for-each (match-lambda (($ <global> id type init)
                              (add-global-id! id)))
               globals)
     (for-each (match-lambda (($ <elem> id mode table type offset init)
                              (add-elem-id! id)))
               elems)
     (for-each (match-lambda (($ <data> id mode mem offset init)
                              (add-data-id! id)))
               datas)
     (for-each (match-lambda (($ <tag> id type)
                              (add-tag-id! id)))
               tags)
     (for-each intern-string strings)
     (find-type (lambda (rec type-id type-idx supers type)
                  (match type
                    (($ <struct-type>
                        (($ <field> field-id mutable? type) ...))
                     (for-each
                      (lambda (field-id)
                        (add-struct-field! type-id type-idx field-id))
                      field-id))
                    (_ (values)))
                  #t)
                types)

     (define (type-by-idx idx)
       (or (find-type (lambda (rec type-id type-idx supers type)
                        (and (eqv? type-idx idx)
                             type))
                      types)
           (error "unknown type" idx)))

     (define (resolve-heap-type ht)
       (match ht
         ((or 'func 'extern
              'any 'eq 'i31 'noextern 'nofunc 'struct 'array 'none
              'string 'stringview_wtf8 'stringview_wtf16 'stringview_iter)
          ht)
         (_ (resolve-type ht))))

     (define (resolve-val-type vt)
       (match vt
         ((or 'i32 'i64 'f32 'f64 'v128
              'funcref 'externref 'anyref 'eqref 'i31ref
              'nullexternref 'nullfuncref
              'structref 'arrayref 'nullref
              'stringref
              'stringview_wtf8ref 'stringview_wtf16ref 'stringview_iterref)
          vt)
         (($ <ref-type> nullable? ht)
          (make-ref-type nullable? (resolve-heap-type ht)))))

     (define (resolve-storage-type type)
       (match type
         ((or 'i8 'i16) type)
         (_ (resolve-val-type type))))

     (define (resolve-type-use x)
       (define (lookup-type-use params results)
         (or (find-type (type-use-matcher params results) types)
             (error "unreachable")))
       (match x
         (($ <type-use> idx (and use-sig ($ <func-sig> params results)))
          (if idx
              (let ((idx (resolve-type idx)))
                (match (type-by-idx idx)
                  (($ <type> _ def-sig)
                   (make-type-use idx
                                  (if (and (null? params) (null? results))
                                      def-sig
                                      use-sig)))))
              (or (lookup-type-use params results))))))
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
            (or (list-index (lambda (x) (eqv? x label)) labels)
                (error "unbound label" label labels)))))
       (define (resolve-local id)
         (match id
           ((? exact-integer?) id)
           (_
            (let ((local (list-index
                          (lambda (local)
                            (match local
                              (($ <local> id* _) (eqv? id id*))
                              (($ <param> id* _) (eqv? id id*))
                              (_ #f)))
                          locals)))
              (unless local
                (error "unbound local" id locals))
              local))))
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
          `(,inst ,(resolve-global global)))
         (('string.const (? string? str))
          `(string.const ,(intern-string str)))
         ;; fixme: tables, bulk memory, reftypes...
         (inst inst))
        insts))

     (define (visit-type type)
       (define (resolve-param param)
         (match param
           (($ <param> id type)
            (make-param id (resolve-val-type type)))))
       (define (resolve-field field)
         (match field
           (($ <field> id mutable? type)
            (make-field id mutable? (resolve-storage-type type)))))
       (define (resolve-base type)
         (match type
           (($ <func-sig> params results)
           (make-func-sig (map resolve-param params)
                          (map resolve-val-type results)))
           (($ <array-type> mutable? type)
            (make-array-type mutable? (resolve-storage-type type)))
           (($ <struct-type> fields)
            (make-struct-type (map resolve-field fields)))))
       (define (resolve-sub type)
         (match type
           (($ <type> id type)
            (make-type id
                       (match type
                         (($ <sub-type> supers type)
                          (make-sub-type (map resolve-heap-type supers)
                                         (resolve-base type)))
                         (_ (resolve-base type)))))))
       (match type
         (($ <rec-group> sub-types)
          (make-rec-group (map resolve-sub sub-types)))
         (_ (resolve-sub type))))

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
          (match (resolve-type-use type)
            ((and type ($ <type-use> _ ($ <func-sig> params _)))
             (make-func id type locals
                        (resolve-instructions body
                                              (append params locals)
                                              '())))))))

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

     (define (visit-tag tag)
       (match tag
         (($ <tag> id type)
          (make-tag id (resolve-type-use type)))))

     ;; FIXME: reftypes means we have to resolve types in more places

     (let ((types (map visit-type types))
           (imports (map visit-import imports))
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
       (make-wasm types imports funcs tables memories globals exports start
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
      (match inst
        ((op args ...) (values op args))
        (op (values op '()))))

    (define (emit code)
      (match args
        (() (emit-u8 port code))
        (_ (bad-instruction))))
    (define (emit-block code)
      (match args
        ((label bt insts)
         (emit-u8 port code)
         (emit-block-type port bt)
         (emit-instructions port insts)
         (emit-end port))
        (_ (bad-instruction))))
    (define (emit-if code)
      (define else-code #x05)
      (match args
        ((label bt consequent alternate)
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
    (define (emit-misc-op code)
      (emit-u8 port #xfc)
      (put-uleb port code))
    (define (emit-misc code)
      (match args
        (()
         (emit-misc-op code))
        (_ (bad-instruction))))
    (define (emit-misc-idx code)
      (match args
        ((idx)
         (emit-misc-op code)
         (emit-u32 port idx))
        (_ (bad-instruction))))
    (define (emit-misc-idx-idx code)
      (match args
        ((idx0 idx1)
         (emit-misc-op code)
         (emit-u32 port idx0)
         (emit-u32 port idx1))
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
      ('table.get           (emit-idx #x25))
      ('table.set           (emit-idx #x26))
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

      ;; Misc opcodes.
      ('i32.trunc_sat_f32_s                (emit-misc #x00))
      ('i32.trunc_sat_f32_u                (emit-misc #x01))
      ('i32.trunc_sat_f64_s                (emit-misc #x02))
      ('i32.trunc_sat_f64_u                (emit-misc #x03))
      ('i64.trunc_sat_f32_s                (emit-misc #x04))
      ('i64.trunc_sat_f32_u                (emit-misc #x05))
      ('i64.trunc_sat_f64_s                (emit-misc #x06))
      ('i64.trunc_sat_f64_u                (emit-misc #x07))
      ('memory.init                        (emit-misc-idx-idx #x08))
      ('data.drop                          (emit-misc-idx #x09))
      ('memory.copy                        (emit-misc-idx-idx #x0a))
      ('memory.fill                        (emit-misc-idx #x0b))
      ('table.init                         (emit-misc-idx-idx #x0c))
      ('elem.drop                          (emit-misc-idx #x0d))
      ('table.copy                         (emit-misc-idx-idx #x0e))
      ('table.grow                         (emit-misc-idx #x0f))
      ('table.size                         (emit-misc-idx #x10))
      ('table.fill                         (emit-misc-idx #x11))

      (_ (bad-instruction))))

  (define (emit-instructions port insts)
    (for-each (lambda (inst) (emit-instruction port inst)) insts))

  (define (emit-expr port expr)
    (emit-instructions port expr)
    (emit-end port))

  (define (emit-type-def port def)
    (define (emit-field-type port mutable? st)
      (match st
        ('i8 (emit-u8 port #x7a))
        ('i16 (emit-u8 port #x79))
        (_ (emit-val-type port st)))
      (emit-u8 port (if mutable? 1 0)))
    (define (emit-field port field)
      (match field
        (($ <field> id mutable? type)
         (emit-field-type port mutable? type))))
    (define (emit-base-type-def port def)
      (match def
        (($ <func-sig> (($ <param> _ param-type) ...) (result-type ...))
         (emit-u8 port #x60)
         (emit-result-type port param-type)
         (emit-result-type port result-type))
        (($ <struct-type> fields)
         (emit-u8 port #x5f)
         (emit-vec port fields emit-field))
        (($ <array-type> mutable? type)
         (emit-u8 port #x5e)
         (emit-field-type port mutable? type))))
    (define (emit-sub-type-def port def)
      (match def
        (($ <sub-type> supers def)
         (emit-u8 port #x50)
         (emit-vec port supers emit-u32)
         (emit-base-type-def port def))
        (_ (emit-base-type-def port def))))
    (match def
      (($ <rec-group> (($ <type> _ def) ...))
       (emit-u8 port #x4f)
       (emit-vec port def emit-sub-type-def))
      (($ <type> id def)
       (emit-sub-type-def port def))))

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
         ('memory
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
                (acons (1+ count) type compressed))
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
        (put-bytevector port #vu8(1 0 0 0))             ;; version
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
        (emit-vec-section port 9 elems emit-element)
        (unless (null? datas)
          (emit-section port 12 (call-with-output-bytevector
                                 (lambda (port)
                                   (emit-u32 port (length datas))))))
        (emit-vec-section port 10 funcs emit-func-def)
        (emit-vec-section port 11 datas emit-data))))))

(define (wat->wasm expr)
  (assemble-wasm (resolve-wasm (parse-wat expr))))
