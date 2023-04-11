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

(define-module (wasm resolve)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (list-index))
  #:use-module (srfi srfi-11)
  #:use-module (wasm types)
  #:export (resolve-wasm))

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

(define (resolve-wasm mod)
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
    (if (exact-integer? field)
        field
        (match (hashq-ref struct-fields struct-id-or-idx)
          ((add-id! . resolve-id)
           (resolve-id field)))))

  (define interned-strings (make-hash-table))
  (define interned-string-count 0)
  (define (intern-string string)
    (or (hash-ref interned-strings string)
        (let ((idx interned-string-count))
          (hash-set! interned-strings string idx)
          (set! interned-string-count (1+ idx))
          idx)))

  (define functions-used-as-values (make-hash-table))
  (define (record-function-used-as-value idx)
    (unless (exact-integer? idx) (error "expected resolved idx"))
    (hashv-set! functions-used-as-values idx #t)
    idx)

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
        (($ <import>) types)))
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
          (('try_delegate label type body handler)
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
       ;; Transform symbolic or anonymous type uses to indexed type
       ;; uses.  No need to resolve value types for params or results;
       ;; that's the job for `visit-type`.
       (define (lookup-type-use params results)
         (or (find-type (type-use-matcher params results) types)
             (error "unreachable")))
       (match x
         (($ <type-use> idx (and use-sig ($ <func-sig> params results)))
          (if idx
              (let ((idx (resolve-type idx)))
                (let ((def-sig (type-by-idx idx)))
                  (make-type-use idx
                                 (if (and (null? params) (null? results))
                                     def-sig
                                     use-sig))))
              (or (lookup-type-use params results))))))

     (define (resolve-type-use-as-idx x)
       (match (resolve-type-use x)
         (($ <type-use> idx func-sig)
          idx)))

     (define (resolve-block-type x)
       (match x
         (($ <type-use> #f ($ <func-sig> () (or () (_))))
          x)
         (_ (resolve-type-use-as-idx x))))

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
         (('try label type body catches catch-all)
          (let ((labels (cons label labels)))
            `(try ,label ,(resolve-block-type type)
                  ,(resolve-instructions body locals labels)
                  ,(map (lambda (body)
                          (resolve-instructions body locals labels))
                        catches)
                  ,(and catch-all
                        (resolve-instructions catch-all locals labels)))))
         (('try_delegate label type body handler)
          (let ((labels (cons label labels)))
            `(try_delegate ,label ,(resolve-block-type type)
                           ,(resolve-instructions body locals labels)
                           ,(resolve-label handler))))
         (((and inst (or 'throw 'rethrow)) tag) `(,inst ,(resolve-tag tag)))
         (((and inst (or 'br 'br_if 'br_on_null 'br_on_non_null)) label)
          `(,inst ,(resolve-label label)))
         (('br_table targets default)
          `(br_table ,(map resolve-label targets) ,(resolve-label default)))
         (((and inst (or 'call 'return_call)) label)
          `(,inst ,(resolve-func label)))
         (('call_indirect table type)
          `(call_indirect ,(resolve-table table) ,(resolve-type-use-as-idx type)))
         (((and inst (or 'call_ref 'return_call_ref)) type)
          `(,inst ,(resolve-type-use-as-idx type)))
         (('select types) `(select ,(map resolve-val-type types)))
         (((and inst (or 'local.get 'local.set 'local.tee)) local)
          `(,inst ,(resolve-local local)))
         (((and inst (or 'global.get 'global.set)) global)
          `(,inst ,(resolve-global global)))
         (((and inst (or 'table.get 'table.set)) table)
          `(,inst ,(resolve-table table)))
         (((and inst (or 'memory.size 'memory.grow)) mem)
          `(,inst ,(resolve-memory mem)))
         (('ref.null ht) `(ref.null ,(resolve-heap-type ht)))
         (('ref.func f) `(ref.func ,(record-function-used-as-value
                                     (resolve-func f))))

         ;; GC instructions.
         (((and inst (or 'struct.get 'struct.get_s 'struct.get_u 'struct.set))
           type field)
          `(,inst ,(resolve-type type) ,(resolve-struct-field type field)))
         (((and inst (or 'struct.new 'struct.new_default)) type)
          `(,inst ,(resolve-type type)))
         (((and inst (or 'array.get 'array.get_s 'array.get_u 'array.set)) type)
          `(,inst ,(resolve-type type)))
         (('array.new_fixed type len)
          `(array.new_fixed ,(resolve-type type) ,len))
         (((and inst (or 'array.new 'array.new_default)) type)
          `(,inst ,(resolve-type type)))
         (('array.new_data type data)
          `(array.new_fixed ,(resolve-type type) ,(resolve-data data)))
         (('array.new_elem type elem)
          `(array.new_fixed ,(resolve-type type) ,(resolve-elem elem)))
         (('array.copy dst src)
          `(array.copy ,(resolve-type dst) ,(resolve-type src)))
         (((and inst (or 'ref.test 'ref.cast)) nullable? ht)
          `(,inst ,nullable? ,(resolve-heap-type ht)))
         (('string.const (? string? str))
          `(string.const ,(intern-string str)))
         (((and inst (or 'string.new_utf8 'string.new_lossy_utf8 'string.new_wtf8
                         'string.new_wtf16
                         'string.encode_utf8 'string.encode_lossy_utf8
                         'string.encode_wtf8 'string.encode_wtf16
                         'stringview_wtf8.encode_utf8
                         'stringview_wtf8.encode_lossy_utf8
                         'stringview_wtf8.encode_wtf8
                         'stringview_wtf16.encode))
           mem)
          `(,inst ,(resolve-memory mem)))

         ;; Misc instructions.
         (('memory.init data mem)
          `(memory.init ,(resolve-data data) ,(resolve-memory mem)))
         (('data.drop data)
          `(data.drop ,(resolve-data data)))
         (('memory.copy dst src)
          `(memory.copy ,(resolve-memory dst) ,(resolve-memory src)))
         (('memory.fill mem)
          `(memory.fill ,(resolve-memory mem)))
         (('table.init elem table)
          `(table.init ,(resolve-elem elem) ,(resolve-table table)))
         (('elem.drop elem)
          `(elem.drop ,(resolve-elem elem)))
         (('table.copy dst src)
          `(table.copy ,(resolve-table dst) ,(resolve-table src)))
         (((and inst (or 'table.grow 'table.size 'table.fill)) table)
          `(,inst ,(resolve-table table)))
         
         ;; Not yet implemented: simd mem ops, atomic mem ops.

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
         (($ <import> mod name 'global id ($ <global-type> mutable? type))
          (make-import mod name 'global id
                       (make-global-type mutable? (resolve-val-type type))))
         ((and import ($ <import> mod name 'memory))
          import)
         (($ <import> mod name 'table id ($ <table-type> limits type))
          (make-import mod name 'table id
                       (make-table-type limits (resolve-val-type type))))))

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

     (define (strip-declarative-segments elems)
       (filter (match-lambda
                (($ <elem> id mode) (not (eq? mode 'declarative))))
               elems))
     (define (add-declarative-segment elems)
       (match (sort (hash-map->list (lambda (k v) k) functions-used-as-values)
                    <)
         (() elems)
         (funcs
          (let ((declarative (make-elem #f 'declarative #f 'funcref #f
                                        (map (lambda (func-idx)
                                               `((ref.func ,func-idx)))
                                             funcs))))
            (append elems (list declarative))))))

     (define (visit-elem elem)
       (match elem
         (($ <elem> id mode table type offset init)
          (make-elem id mode (and table (resolve-table table))
                     (resolve-val-type type)
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
       (define (visit-local local)
         (match local
           (($ <local> id type)
            (make-local id (resolve-val-type type)))))
       (match func
         (($ <func> id type locals body)
          (match (resolve-type-use type)
            ((and type ($ <type-use> idx ($ <func-sig> params _)))
             (make-func id type (map visit-local locals)
                        (resolve-instructions body
                                              (append params locals)
                                              '())))))))

     (define (visit-table table)
       (match table
         (($ <table> id ($ <table-type> limits type) init)
          (make-table id
                      (make-table-type limits (resolve-val-type type))
                      (and init (resolve-instructions init '() '()))))))

     (define (visit-memory mem) mem)

     (define (visit-global global)
       (match global
         (($ <global> id ($ <global-type> mutable? type) init)
          (make-global id
                       (make-global-type mutable? (resolve-val-type type))
                       (resolve-instructions init '() '())))))

     (define (visit-tag tag)
       (match tag
         (($ <tag> id type)
          (make-tag id (resolve-type-use type)))))

     (let ((types (map visit-type types))
           (imports (map visit-import imports))
           (exports (map visit-export exports))
           (%elems (map visit-elem (strip-declarative-segments elems)))
           (datas (map visit-data datas))
           (start (visit-start start))
           (funcs (map visit-func funcs))
           (tables (map visit-table tables))
           (memories (map visit-memory memories))
           (globals (map visit-global globals))
           (tags (map visit-tag tags)))
       (define strings
         (map car
              (sort (hash-map->list cons interned-strings)
                    (match-lambda*
                     (((s1 . idx1) (s2 . idx2)) (< idx1 idx2))))))
       (define elems (add-declarative-segment %elems))
       (make-wasm types imports funcs tables memories globals exports start
                  elems datas tags strings custom)))))
