;;; WebAssembly resolver
;;; Copyright (C) 2023 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
;;; Copyright (C) 2023, 2024 David Thompson <dave@spritely.institute>
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
;;; Lowers WASM with human readable identifiers to WASM with only
;;; index references.
;;;
;;; Code:

(define-module (wasm resolve)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (find list-index))
  #:use-module (srfi srfi-11)
  #:use-module (wasm types)
  #:export (resolve-wasm
            unresolve-wasm))

(define (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define (alist-sort alist)
  (sort alist (lambda (a b) (< (car a) (car b)))))

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
               (else (error "unbound identifier" id))))
            (lambda ()
              (alist-sort
               (hash-fold (lambda (name idx result)
                            (cons (cons idx name) result))
                          '() ids))))))

(define (make-indirect-name-store)
  (let ((table (make-hash-table)))
    (values (lambda (parent-id parent-idx id)
              (match (hashq-ref table parent-idx)
                (#f
                 (let-values (((add-id! resolve-id name-map) (make-name-store)))
                   (let ((procs (list add-id! resolve-id name-map)))
                     (hashq-set! table parent-idx procs)
                     (when parent-id
                       (hashq-set! table parent-id procs)))
                   (add-id! id)))
                ((add-id! resolve-id name-map)
                 (add-id! id))))
            (lambda (parent-id-or-idx id-or-idx)
              (if (exact-integer? id-or-idx)
                  id-or-idx
                  (match (hashq-ref table parent-id-or-idx)
                    ((add-id! resolve-id name-map)
                     (resolve-id id-or-idx)))))
            (lambda ()
              (alist-sort
               (hash-fold
                (lambda (id-or-idx procs result)
                  (if (exact-integer? id-or-idx)
                      (match procs
                        ((_ _ name-map)
                         (match (name-map)
                           ((name-map ..1) (cons (cons id-or-idx name-map) result))
                           (_ result))))
                      result))
                '()
                table))))))

(define* (resolve-wasm mod #:key name-section?)
  (define-values (add-type-id! resolve-type type-name-map) (make-name-store))
  (define-values (add-func-id! resolve-func func-name-map) (make-name-store))
  (define-values (add-table-id! resolve-table table-name-map) (make-name-store))
  (define-values (add-memory-id! resolve-memory memory-name-map) (make-name-store))
  (define-values (add-global-id! resolve-global global-name-map) (make-name-store))
  (define-values (add-elem-id! resolve-elem elem-name-map) (make-name-store))
  (define-values (add-data-id! resolve-data data-name-map) (make-name-store))
  (define-values (add-tag-id! resolve-tag tag-name-map) (make-name-store))
  (define-values (add-struct-field! resolve-struct-field struct-field-name-map)
    (make-indirect-name-store))
  (define-values (add-func-local! resolve-func-local func-local-name-map)
    (make-indirect-name-store))
  (define-values (add-func-label! resolve-func-label func-label-name-map)
    (make-indirect-name-store))
  (define (add-func-locals! func)
    (match func
      (($ <func> id ($ <type-use> _ type) locals)
       (let ((idx (resolve-func id)))
         (for-each (lambda (local-id)
                     (add-func-local! id idx local-id))
                   (append (map param-id (func-sig-params type))
                           (map local-id locals)))))))
  (define (add-func-labels! func)
    (match func
      (($ <func> id _ _ body)
       (let ((idx (resolve-func id)))
         (let loop ((insts body))
           (match insts
             (() #t)
             ((((or 'block 'loop) label _ body) . rest)
              (add-func-label! id idx label)
              (loop body)
              (loop rest))
             ((('if label _ consequent alternate) . rest)
              (add-func-label! id idx label)
              (loop consequent)
              (loop alternate)
              (loop rest))
             ((_ . rest)
              (loop rest))))))))

  (define (resolve-memarg memarg)
    (match memarg
      (($ <mem-arg> id offset align)
       (make-mem-arg (resolve-memory id) offset align))))

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
                   (make-type-use type-idx (make-func-sig params results))))
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
    (($ <wasm> id %types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (define (generate-names)
       (make-names id
                   (func-name-map)
                   (func-local-name-map)
                   (func-label-name-map)
                   (type-name-map)
                   (table-name-map)
                   (memory-name-map)
                   (global-name-map)
                   (elem-name-map)
                   (data-name-map)
                   (struct-field-name-map)
                   (tag-name-map)))
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
                  #f)
                types)
     (when name-section?
       (for-each add-func-locals! funcs)
       (for-each add-func-labels! funcs))

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

     (define (resolve-ref-type rt)
       (resolve-val-type rt))

     (define (resolve-storage-type type)
       (match type
         ((or 'i8 'i16) type)
         (_ (resolve-val-type type))))

     (define (resolve-param param)
       (match param
         (($ <param> id type)
          (make-param id (resolve-val-type type)))))

     (define (resolve-type-use x)
       ;; Transform symbolic or anonymous type uses to indexed type
       ;; uses.
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
              (match (lookup-type-use params results)
                (($ <type-use> idx ($ <func-sig> params results))
                 (let ((params (map resolve-param params))
                       (results (map resolve-val-type results)))
                   (make-type-use idx (make-func-sig params results)))))))))

     (define (resolve-type-use-as-idx x)
       (match (resolve-type-use x)
         (($ <type-use> idx func-sig)
          idx)))

     (define (resolve-block-type x)
       (match x
         (($ <type-use> #f ($ <func-sig> () ()))
          x)
         (($ <type-use> #f ($ <func-sig> () (ret)))
          (let ((ret (resolve-val-type ret)))
            (make-type-use #f (make-func-sig '() (list ret)))))
         (_ (resolve-type-use-as-idx x))))

     (define (resolve-instructions insts locals labels)
       (define (resolve-i32 x)
         (if (< x (ash 1 31)) x (- x (ash 1 32))))
       (define (resolve-i64 x)
         (if (< x (ash 1 63)) x (- x (ash 1 64))))
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
         (((and inst (or 'br 'br_if)) label)
          `(,inst ,(resolve-label label)))
         (('br_table targets default)
          `(br_table ,(map resolve-label targets) ,(resolve-label default)))
         (((and inst (or 'call 'return_call)) label)
          `(,inst ,(resolve-func label)))
         (('call_indirect table type)
          `(call_indirect ,(resolve-table table) ,(resolve-type-use-as-idx type)))
         (((and inst (or 'call_ref 'return_call_ref)) type)
          `(,inst ,(resolve-type type)))
         (('select types) `(select ,(map resolve-val-type types)))
         (((and inst (or 'local.get 'local.set 'local.tee)) local)
          `(,inst ,(resolve-local local)))
         (((and inst (or 'global.get 'global.set)) global)
          `(,inst ,(resolve-global global)))
         (((and inst (or 'table.get 'table.set)) table)
          `(,inst ,(resolve-table table)))
         (((and inst (or 'memory.size 'memory.grow)) mem)
          `(,inst ,(resolve-memory mem)))
         (((and inst (or 'i32.load 'i64.load 'f32.load 'f64.load
                         'i32.load8_s 'i32.load8_u 'i32.load16_s 'i32.load16_u
                         'i64.load8_s 'i64.load8_u 'i64.load16_s 'i64.load16_u
                         'i64.load32_s 'i64.load32_u
                         'i32.store 'i64.store 'f32.store 'f64.store
                         'i32.store8 'i32.store16
                         'i64.store8 'i64.store16 'i64.store32))
           mem)
          `(,inst ,(resolve-memarg mem)))
         (('i32.const x) `(i32.const ,(resolve-i32 x)))
         (('i64.const x) `(i64.const ,(resolve-i64 x)))
         (('ref.null ht) `(ref.null ,(resolve-heap-type ht)))
         (('ref.func f) `(ref.func ,(record-function-used-as-value
                                     (resolve-func f))))

         ;; GC instructions.
         (((and inst (or 'ref.test 'ref.cast)) rt)
          `(,inst ,(resolve-ref-type rt)))
         (((and inst (or 'br_on_cast 'br_on_cast_fail)) label rt1 rt2)
          `(,inst ,(resolve-label label)
                  ,(resolve-ref-type rt1) ,(resolve-ref-type rt2)))
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
         (((and inst (or 'array.new_data 'array.init_data)) type data)
          `(,inst ,(resolve-type type) ,(resolve-data data)))
         (((and inst (or 'array.new_elem 'array.init_elem)) type elem)
          `(,inst ,(resolve-type type) ,(resolve-elem elem)))
         (('array.fill type)
          `(array.fill ,(resolve-type type)))
         (('array.copy dst src)
          `(array.copy ,(resolve-type dst) ,(resolve-type src)))

         ;; Stringref instructions.
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
          `(,inst ,(resolve-memarg mem)))

         ;; Misc instructions.
         (('memory.init data mem)
          `(memory.init ,(resolve-data data) ,(resolve-memory mem)))
         (('data.drop data)
          `(data.drop ,(resolve-data data)))
         (('memory.copy dst src)
          `(memory.copy ,(resolve-memory dst) ,(resolve-memory src)))
         (('memory.fill mem)
          `(memory.fill ,(resolve-memory mem)))
         (('table.init table elem)
          `(table.init ,(resolve-table table) ,(resolve-elem elem)))
         (('elem.drop elem)
          `(elem.drop ,(resolve-elem elem)))
         (('table.copy dst src)
          `(table.copy ,(resolve-table dst) ,(resolve-table src)))
         (((and inst (or 'table.grow 'table.size 'table.fill)) table)
          `(,inst ,(resolve-table table)))

         ;; Not yet implemented: simd mem ops, atomic mem ops.

         ((? symbol? op) `(,op))
         (inst inst))
        insts))

     (define (visit-type type)
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
                         (($ <sub-type> final? supers type)
                          (make-sub-type final?
                                         (map resolve-heap-type supers)
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
                     (and offset (resolve-instructions offset '() '()))
                     (map (lambda (init)
                            (resolve-instructions init '() '()))
                          init)))))

     (define (visit-data data)
       (match data
         (($ <data> id mode mem offset init)
          (make-data id mode (and mem (resolve-memory mem))
                     (and offset (resolve-instructions offset '() '()))
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
           (tags (map visit-tag tags))
           (custom (if name-section?
                       (cons (generate-names) custom)
                       custom)))
       (define strings
         (map car
              (sort (hash-map->list cons interned-strings)
                    (match-lambda*
                     (((s1 . idx1) (s2 . idx2)) (< idx1 idx2))))))
       (define elems (add-declarative-segment %elems))
       (make-wasm #f types imports funcs tables memories globals exports start
                  elems datas tags strings custom)))))

(define (unresolve-wasm mod)
  (match mod
    (($ <wasm> id types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (match (or (find names? custom)
                (make-names #f '() '() '() '() '() '() '() '() '() '() '()))
       (($ <names> mod-name func-names local-names label-names type-names
           table-names memory-names global-names elem-names data-names
           field-names tag-names)
        (define (make-id prefix idx)
          (string->symbol
           (string-append "$" prefix (number->string idx))))
        (define (name-generator prefix)
          (lambda (idx)
            (make-id prefix idx)))
        (define (unresolver* name-map fallback)
          (lambda (idx)
            (or (assq-ref name-map idx)
                (fallback idx))))
        (define (unresolver name-map prefix)
          (unresolver* name-map (name-generator prefix)))
        (define (indirect-ref name-map parent-idx idx)
          (assq-ref (or (assq-ref name-map parent-idx) '()) idx))
        (define (indirect-unresolver* name-map fallback)
          (lambda (parent-idx idx)
            (or (indirect-ref name-map parent-idx idx)
                (fallback idx))))
        (define (indirect-unresolver name-map prefix)
          (indirect-unresolver* name-map (name-generator prefix)))
        (define unresolve-func (unresolver func-names "func"))
        (define unresolve-local (indirect-unresolver local-names "var"))
        (define unresolve-label (indirect-unresolver* label-names (const #f)))
        (define unresolve-type (unresolver type-names "type"))
        (define unresolve-table (unresolver table-names "table"))
        (define unresolve-memory (unresolver memory-names "memory"))
        (define unresolve-global (unresolver global-names "global"))
        (define unresolve-elem (unresolver elem-names "elem"))
        (define unresolve-data (unresolver data-names "data"))
        (define unresolve-field (indirect-unresolver field-names "field"))
        (define unresolve-tag (unresolver tag-names "tag"))

        (define (mapi proc i lst)
          (let loop ((lst lst) (i i))
            (match lst
              (() '())
              ((x . rest)
               (cons (proc i x) (loop rest (+ i 1)))))))

        (define (unresolve-param param)
          (match param
            (($ <param> id type)
             (make-param id (unresolve-val-type type)))))

        (define (unresolve-instructions insts func-idx)
          (define (unresolve-ref-type rt)
            (match rt
              (($ <ref-type> nullable? ht)
               (make-ref-type nullable? (unresolve-heap-type ht)))))
          (define (unresolve-block-type type)
            (match type
              (#f #f)
              ((? type-use? type)
               (unresolve-type-use func-idx type))
              (_ (unresolve-val-type type))))
          (define (unresolve-memarg memarg)
            (match memarg
              (($ <mem-arg> idx offset align)
               (make-mem-arg (unresolve-memory idx) offset align))))
          (define label-count 0)
          (define (next-label)
            (let ((l label-count))
              (set! label-count (+ label-count 1))
              l))
          (define (unresolve-instructions* insts labels)
            (define (unresolve-label/block idx)
              (or (list-ref labels idx) idx))
            (map
             (match-lambda
               (((and inst (or 'block 'loop)) label type body)
                (let* ((label (unresolve-label func-idx (next-label)))
                       (labels (cons label labels)))
                  `(,inst ,label
                          ,(unresolve-block-type type)
                          ,(unresolve-instructions* body labels))))
               (('if label type consequent alternate)
                (let ((labels (cons label labels)))
                  `(if ,label
                       ,(unresolve-block-type type)
                       ,(unresolve-instructions* consequent labels)
                       ,(unresolve-instructions* alternate labels))))
               (('try label type body catches catch-all)
                (let* ((label (unresolve-label func-idx (next-label)))
                       (labels (cons label labels)))
                  `(try ,label
                        ,(unresolve-block-type type)
                        ,(unresolve-instructions* body labels)
                        ,(map (lambda (body)
                                (unresolve-instructions* body labels))
                              catches)
                        ,(and catch-all
                              (unresolve-instructions* catch-all labels)))))
               (('try_delegate label type body handler)
                (let* ((label (unresolve-label func-idx (next-label)))
                       (labels (cons label labels)))
                  `(try_delegate ,label
                                 ,(unresolve-block-type type)
                                 ,(unresolve-instructions* body labels)
                                 ,(unresolve-label func-idx handler))))
               (((and inst (or 'throw 'rethrow)) tag)
                `(,inst ,(unresolve-tag tag)))
               (((and inst (or 'br 'br_if)) label)
                `(,inst ,(unresolve-label/block label)))
               (('br_table targets default)
                `(br_table ,@(map unresolve-label/block targets)
                           ,(unresolve-label/block default)))
               (((and inst (or 'call 'return_call)) func)
                `(,inst ,(unresolve-func func)))
               (('call_indirect table type)
                `(call_indirect ,(unresolve-table table) ,(unresolve-type-use #f type)))
               (((and inst (or 'call_ref 'return_call_ref)) type)
                `(,inst ,(unresolve-type type)))
               (('select types) `(select ,(map unresolve-val-type types)))
               (((and inst (or 'local.get 'local.set 'local.tee)) local)
                `(,inst ,(unresolve-local func-idx local)))
               (((and inst (or 'global.get 'global.set)) global)
                `(,inst ,(unresolve-global global)))
               (((and inst (or 'table.get 'table.set)) table)
                `(,inst ,(unresolve-table table)))
               (((and inst (or 'memory.size 'memory.grow)) mem)
                `(,inst ,(unresolve-memory mem)))
               (((and inst (or 'i32.load 'i64.load 'f32.load 'f64.load
                               'i32.load8_s 'i32.load8_u 'i32.load16_s 'i32.load16_u
                               'i64.load8_s 'i64.load8_u 'i64.load16_s 'i64.load16_u
                               'i64.load32_s 'i64.load32_u
                               'i32.store 'i64.store 'f32.store 'f64.store
                               'i32.store8 'i32.store16
                               'i64.store8 'i64.store16 'i64.store32))
                 mem)
                `(,inst ,(unresolve-memarg mem)))
               (('ref.null ht) `(ref.null ,(unresolve-heap-type ht)))
               (('ref.func f) `(ref.func ,(unresolve-func f)))

               ;; GC instructions.
               (((and inst (or 'ref.test 'ref.cast)) rt)
                `(,inst ,(unresolve-ref-type rt)))
               (((and inst (or 'br_on_cast 'br_on_cast_fail)) label rt1 rt2)
                `(,inst ,(unresolve-label func-idx label)
                        ,(unresolve-ref-type rt1) ,(unresolve-ref-type rt2)))
               (((and inst (or 'struct.get 'struct.get_s 'struct.get_u 'struct.set))
                 type field)
                `(,inst ,(unresolve-type type) ,(unresolve-field type field)))
               (((and inst (or 'struct.new 'struct.new_default)) type)
                `(,inst ,(unresolve-type type)))
               (((and inst (or 'array.get 'array.get_s 'array.get_u 'array.set)) type)
                `(,inst ,(unresolve-type type)))
               (('array.new_fixed type len)
                `(array.new_fixed ,(unresolve-type type) ,len))
               (((and inst (or 'array.new 'array.new_default)) type)
                `(,inst ,(unresolve-type type)))
               (((and inst (or 'array.new_data 'array.init_data)) type data)
                `(,inst ,(unresolve-type type) ,(unresolve-data data)))
               (((and inst (or 'array.new_elem 'array.init_elem)) type elem)
                `(,inst ,(unresolve-type type) ,(unresolve-elem elem)))
               (('array.fill type)
                `(array.fill ,(unresolve-type type)))
               (('array.copy dst src)
                `(array.copy ,(unresolve-type dst) ,(unresolve-type src)))

               ;; Stringref instructions.
               (('string.const idx)
                `(string.const (list-ref strings idx)))
               (((and inst (or 'string.new_utf8 'string.new_lossy_utf8 'string.new_wtf8
                               'string.new_wtf16
                               'string.encode_utf8 'string.encode_lossy_utf8
                               'string.encode_wtf8 'string.encode_wtf16
                               'stringview_wtf8.encode_utf8
                               'stringview_wtf8.encode_lossy_utf8
                               'stringview_wtf8.encode_wtf8
                               'stringview_wtf16.encode))
                 mem)
                `(,inst ,(unresolve-memarg mem)))

               ;; Misc instructions.
               (('memory.init data mem)
                `(memory.init ,(unresolve-data data) ,(unresolve-memory mem)))
               (('data.drop data)
                `(data.drop ,(unresolve-data data)))
               (('memory.copy dst src)
                `(memory.copy ,(unresolve-memory dst) ,(unresolve-memory src)))
               (('memory.fill mem)
                `(memory.fill ,(unresolve-memory mem)))
               (('table.init table elem)
                `(table.init ,(unresolve-table table) ,(unresolve-elem elem)))
               (('elem.drop elem)
                `(elem.drop ,(unresolve-elem elem)))
               (('table.copy dst src)
                `(table.copy ,(unresolve-table dst) ,(unresolve-table src)))
               (((and inst (or 'table.grow 'table.size 'table.fill)) table)
                `(,inst ,(unresolve-table table)))

               ;; Not yet implemented: simd mem ops, atomic mem ops.

               ((? symbol? op) `(,op))
               (inst inst))
             insts))
          (unresolve-instructions* insts '()))

        (define (unresolve-heap-type ht)
          (match ht
            ((or 'func 'extern
                 'any 'eq 'i31 'noextern 'nofunc 'struct 'array 'none
                 'string 'stringview_wtf8 'stringview_wtf16 'stringview_iter)
             ht)
            (_ (unresolve-type ht))))

        (define (unresolve-val-type vt)
          (match vt
            ((or 'i32 'i64 'f32 'f64 'v128
                 'funcref 'externref 'anyref 'eqref 'i31ref
                 'nullexternref 'nullfuncref
                 'structref 'arrayref 'nullref
                 'stringref
                 'stringview_wtf8ref 'stringview_wtf16ref 'stringview_iterref)
             vt)
            (($ <ref-type> nullable? ht)
             (make-ref-type nullable? (unresolve-heap-type ht)))))

        (define (unresolve-storage-type type)
          (match type
            ((or 'i8 'i16) type)
            (_ (unresolve-val-type type))))

        (define (unresolve-type-use func-idx type)
          (match type
            (($ <type-use> idx ($ <func-sig> params results))
             (let ((params (mapi (lambda (idx param)
                                   (match param
                                     (($ <param> id type)
                                      (make-param (unresolve-local func-idx idx)
                                                  (unresolve-val-type type)))))
                                 0 params))
                   (results (map unresolve-val-type results)))
               (make-type-use #f (make-func-sig params results))))))

        ;; During resolution, all anonymous function signatures found
        ;; in block types are added to the types section.  In the
        ;; unresolved output we want to remove these, leaving only
        ;; function types that are referenced by name.
        (define referenced-types (make-hash-table))
        (define (mark-type-reference! idx)
          (hashq-set! referenced-types idx #t))
        (define (type-referenced? idx)
          (hashq-ref referenced-types idx))
        (define (scan-heap-type ht)
          (when (exact-integer? ht)
            (mark-type-reference! ht)))
        (define (scan-val-type vt)
          (match vt
            ((? exact-integer?) (mark-type-reference! vt))
            ((? symbol?) #f)
            (($ <ref-type> nullable? ht)
             (scan-heap-type ht))))
        (define (scan-param param)
          (match param
            (($ <param> id type)
             (scan-val-type type))))
        (define (scan-field field)
          (match field
            (($ <field> id mutable? type)
             (scan-val-type type))))
        (define (scan-type type)
          (match type
            (($ <func-sig> params results)
             (for-each scan-param params)
             (for-each scan-val-type results))
            (($ <sub-type> final? supers type)
             (scan-type type))
            (($ <struct-type> fields)
             (for-each scan-field fields))
            (($ <array-type> mutable? type)
             (scan-val-type type))))
        (define (scan-type-def type)
          (match type
            (($ <type> id type)
             (scan-type type))
            (($ <rec-group> types)
             (for-each (match-lambda
                         (($ <type> id type)
                          (scan-type type)))
                       types))))
        (define (scan-type-use type-use)
          (match type-use
            (($ <type-use> idx type)
             (scan-type type))))
        (define (scan-import import)
          (match import
            (($ <import> mod name 'func id type)
             (scan-type-use type))
            (($ <import> mod name 'table id ($ <table-type> limits type))
             (scan-type type))
            (($ <import> mod name 'memory id type)
             #t)
            (($ <import> mod name 'global id ($ <global-type> mutable? type))
             (scan-type type))))
        (define (scan-elem elem)
          (match elem
            (($ <elem> id mode table type offset inits)
             (scan-val-type type))))
        (define (scan-local local)
          (match local
            (($ <local> id type)
             (scan-val-type type))))
        (define (scan-inst inst)
          (match inst
            (((or 'block 'loop) label type body)
             (scan-body body))
            (('if label type consequent alternate)
             (scan-body consequent)
             (scan-body alternate))
            (('try label type body catches catch-all)
             (scan-body body))
            (('try_delegate label type body handler)
             (scan-body body))
            (((or 'call_indirect 'return_call_indirect) table type)
             (scan-type-use type))
            (_ #t)))
        (define (scan-body body)
          (for-each scan-inst body))
        (define (scan-func func)
          (match func
            (($ <func> id type locals body)
             (scan-type-use type)
             (for-each scan-local locals)
             (scan-body body))))
        (define (scan-table table)
          (match table
            (($ <table> id ($ <table-type> limits elem-type) init)
             (scan-val-type elem-type)
             (when init
               (scan-body init)))))
        (define (scan-global global)
          (match global
            (($ <global> id ($ <global-type> mutable? type) init)
             (scan-val-type type)
             (when init
               (scan-body init)))))
        (define (scan-tag idx tag)
          (match tag
            (($ <tag> id type)
             (scan-type-use type))))
        (for-each scan-type-def types)
        (for-each scan-import imports)
        (for-each scan-elem elems)
        (for-each scan-func funcs)
        (for-each scan-table tables)
        (for-each scan-global globals)
        (for-each scan-tag tags)

        (define (visit-types types)
          (define (unresolve-base idx type)
            (match type
              (($ <func-sig> params results)
               (make-func-sig (map unresolve-param params)
                              (map unresolve-val-type results)))
              (($ <array-type> mutable? type)
               (make-array-type mutable? (unresolve-storage-type type)))
              (($ <struct-type> fields)
               (make-struct-type
                (mapi (lambda (field-idx field)
                        (match field
                          (($ <field> id mutable? type)
                           (make-field (unresolve-field idx field-idx)
                                       mutable?
                                       (unresolve-storage-type type)))))
                      0 fields)))))
          (define (unresolve-sub idx type)
            (match type
              (($ <type> id type)
               (make-type (unresolve-type idx)
                          (match type
                            (($ <sub-type> final? supers type)
                             (make-sub-type final?
                                            (map unresolve-heap-type supers)
                                            (unresolve-base idx type)))
                            (_ (unresolve-base idx type)))))))
          (let loop ((types types) (idx 0))
            (match types
              (() '())
              ((($ <rec-group> types) . rest)
               (cons (make-rec-group (mapi unresolve-sub idx types))
                     (loop rest (+ idx (length types)))))
              (((and type ($ <type> id (? func-sig?))) . rest)
               (if (type-referenced? idx)
                   (cons (unresolve-sub idx type) (loop rest (+ idx 1)))
                   (loop rest (+ idx 1))))
              ((type . rest)
               (cons (unresolve-sub idx type) (loop rest (+ idx 1)))))))

        (define (select-imports kind)
          (filter (lambda (import)
                    (eq? (import-kind import) kind))
                  imports))
        (define func-imports (select-imports 'func))
        (define table-imports (select-imports 'table))
        (define memory-imports (select-imports 'memory))
        (define global-imports (select-imports 'global))
        (define (visit-imports imports)
          (let loop ((imports imports) (func 0) (table 0) (memory 0) (global 0))
            (match imports
              (() '())
              ((($ <import> mod name 'func id type) . rest)
               (cons (make-import mod name 'func (unresolve-func func)
                                  (unresolve-type-use #f type))
                     (loop rest (+ func 1) table memory global)))
              ((($ <import> mod name 'table id ($ <table-type> limits type)) . rest)
               (cons (make-import mod name 'table (unresolve-table table)
                                  (make-table-type limits (unresolve-val-type type)))
                     (loop rest func (+ table 1) memory global)))
              ((($ <import> mod name 'memory id type) . rest)
               (cons (make-import mod name 'memory (unresolve-memory memory) type)
                     (loop rest func table (+ memory 1) global)))
              ((($ <import> mod name 'global id ($ <global-type> mutable? type))
                . rest)
               (cons (make-import mod name 'global (unresolve-global global)
                                  (make-global-type mutable?
                                                    (unresolve-val-type type)))
                     (loop rest func table memory (+ global 1)))))))

        (define (visit-export export)
          (match export
            (($ <export> name 'func idx)
             (make-export name 'func (unresolve-func idx)))
            (($ <export> name 'table idx)
             (make-export name 'table (unresolve-table idx)))
            (($ <export> name 'memory idx)
             (make-export name 'memory (unresolve-memory idx)))
            (($ <export> name 'global idx)
             (make-export name 'global (unresolve-global idx)))))

        (define (visit-elem idx elem)
          (match elem
            (($ <elem> id mode table type offset inits)
             (make-elem (unresolve-elem idx)
                        mode
                        (and table (unresolve-table table))
                        (unresolve-val-type type)
                        (and offset (unresolve-instructions offset #f))
                        (map (lambda (init)
                               (unresolve-instructions init #f))
                             inits)))))

        (define (visit-data idx data)
          (match data
            (($ <data> id mode mem offset init)
             (make-data (unresolve-data idx)
                        mode
                        (and mem (unresolve-memory mem))
                        (and offset (unresolve-instructions offset #f))
                        init))))

        (define (visit-start start)
          (and start (unresolve-func start)))

        (define (visit-func idx func)
          (match func
            (($ <func> id (and type ($ <type-use> _ ($ <func-sig> params)))
                locals body)
             (make-func (unresolve-func idx)
                        (unresolve-type-use idx type)
                        (mapi (lambda (local-idx local)
                                (match local
                                  (($ <local> id type)
                                   (make-local (unresolve-local idx local-idx)
                                               (unresolve-val-type type)))))
                              (length params)
                              locals)
                        (unresolve-instructions body idx)))))

        (define (visit-table idx table)
          (match table
            (($ <table> id ($ <table-type> limits elem-type) init)
             (make-table (unresolve-table idx)
                         (make-table-type limits (unresolve-val-type elem-type))
                         (and init (unresolve-instructions init #f))))))

        (define (visit-memory idx memory)
          (match memory
            (($ <memory> id limits)
             (make-memory (unresolve-memory idx) limits))))

        (define (visit-global idx global)
          (match global
            (($ <global> id ($ <global-type> mutable? type) init)
             (make-global (unresolve-global idx)
                          (make-global-type mutable? (unresolve-val-type type))
                          (unresolve-instructions init #f)))))

        (define (visit-tag idx tag)
          (match tag
            (($ <tag> id type)
             (make-tag (unresolve-tag idx) (unresolve-type-use #f type)))))

        (let ((types (visit-types types))
              (imports (visit-imports imports))
              (exports (map visit-export exports))
              (elems (mapi visit-elem 0 elems))
              (datas (mapi visit-data 0 datas))
              (start (visit-start start))
              (funcs (mapi visit-func (length func-imports) funcs))
              (tables (mapi visit-table (length table-imports) tables))
              (memories (mapi visit-memory (length memory-imports) memories))
              (globals (mapi visit-global (length global-imports) globals))
              (tags (mapi visit-tag 0 tags)))
          (make-wasm mod-name types imports funcs tables memories globals exports start
                     elems datas tags strings custom)))))))
