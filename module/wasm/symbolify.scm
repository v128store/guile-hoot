;;; Replace indices with symbolic identifiers
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
;;; Symbolifier.  The idea is that when optimizing or transforming a
;;; WebAssembly module, often you will want to renumber locals,
;;; functions, or what-not; it's easiest to do that when all references
;;; are symbolic, and then you can just let (wasm resolve) assign
;;; indices.
;;;
;;; Code:

(define-module (wasm symbolify)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map filter-map))
  #:use-module (srfi srfi-11)
  #:use-module (wasm types)
  #:export (symbolify-wasm))

(define (make-gensym stem names)
  (define counter 0)
  (define (gensym)
    (let ((sym (string->symbol (format #f "~a~a" stem counter))))
      (set! counter (1+ counter))
      (if (hashq-ref names sym)
          (gensym)
          sym)))
  gensym)

(define (symbolify-defs wasm)
  (match wasm
    (($ <wasm> types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (define (make-namer stem)
       (define known-names (make-hash-table))
       (define gensym (make-gensym stem known-names))
       (values (lambda (name) (or name (gensym)))
               (lambda (name)
                 (when name
                   (when (hashq-ref known-names name)
                     (error "duplicate name!" name))
                   (hashq-set! known-names name #t)))))
     (define-values (type-name   add-type-name!)   (make-namer "$type"))
     (define-values (func-name   add-func-name!)   (make-namer "$func"))
     (define-values (table-name  add-table-name!)  (make-namer "$table"))
     (define-values (memory-name add-memory-name!) (make-namer "$memory"))
     (define-values (global-name add-global-name!) (make-namer "$global"))
     (define-values (elem-name   add-elem-name!)   (make-namer "$elem"))
     (define-values (data-name   add-data-name!)   (make-namer "$data"))
     (define-values (tag-name    add-tag-name!)    (make-namer "$tag"))

     (for-each (match-lambda
                (($ <rec-group> (($ <type> id) ...))
                 (for-each add-type-name! id))
                (($ <type> id) (add-type-name! id)))
               types)
     (for-each (match-lambda
                (($ <import> mod name 'func   id kind) (add-func-name! id))
                (($ <import> mod name 'table  id kind) (add-table-name! id))
                (($ <import> mod name 'memory id kind) (add-memory-name! id))
                (($ <import> mod name 'global id kind) (add-global-name! id)))
               imports)
     (for-each (match-lambda (($ <func>   id) (add-func-name!   id))) funcs)
     (for-each (match-lambda (($ <table>  id) (add-table-name!  id))) tables)
     (for-each (match-lambda (($ <memory> id) (add-memory-name! id))) memories)
     (for-each (match-lambda (($ <global> id) (add-global-name! id))) globals)
     (for-each (match-lambda (($ <elem>   id) (add-elem-name!   id))) elems)
     (for-each (match-lambda (($ <data>   id) (add-data-name!   id))) datas)
     (for-each (match-lambda (($ <tag>    id) (add-tag-name!    id))) tags)

     (define (ensure-local-names type locals)
       (match type
         (($ <type-use> type-id
             ($ <func-sig> (($ <param> param-id param-type) ...)
                results))
          (match locals
            ((($ <local> local-id local-type) ...)
             (define-values (local-name add-local-name!)
               (make-namer "$_"))
             (for-each add-local-name! param-id)
             (for-each add-local-name! local-id)
             (let* ((params (map make-param
                                 (map local-name param-id)
                                 param-type))
                    (type (make-type-use type-id
                                         (make-func-sig params results)))
                    (locals (map make-local (map local-name local-id)
                                 local-type)))
               (values type locals)))))))

     (let ((types (map (match-lambda
                        (($ <rec-group> (($ <type> id type) ...))
                         (make-rec-group
                          (map make-type (map type-name id) type)))
                        (($ <type> id type)
                         (make-type (type-name id) type)))
                       types))
           (imports (map (match-lambda
                          (($ <import> mod name kind id type)
                           (let ((id (match kind
                                       ('func (func-name id))
                                       ('table (table-name id))
                                       ('memory (memory-name id))
                                       ('global (global-name id)))))
                             (make-import mod name kind id type))))
                         imports))
           (funcs (map (match-lambda
                        (($ <func> id type locals body)
                         (let-values (((type locals)
                                       (ensure-local-names type locals)))
                           (make-func (func-name id) type locals body))))
                       funcs))
           (tables (map (match-lambda
                         (($ <table> id type init)
                          (make-table (table-name id) type init)))
                        tables))
           (memories (map (match-lambda
                           (($ <memory> id type)
                            (make-memory (memory-name id) type)))
                          memories))
           (globals (map (match-lambda
                          (($ <global> id type init)
                           (make-global (global-name id) type init)))
                         globals))
           (elems (map (match-lambda
                        (($ <elem> id mode table type offset inits)
                         (make-elem (elem-name id) mode table type offset
                                    inits)))
                       elems))
           (datas (map (match-lambda
                        (($ <data> id mode mem offset init)
                         (make-data (data-name id) mode mem offset init)))
                       datas))
           (tags (map (match-lambda
                       (($ <tag> id type)
                        (make-tag (tag-name id) type)))
                      tags)))
       (make-wasm types imports funcs tables memories globals exports start
                  elems datas tags strings custom)))))

(define (symbolify-uses wasm)
  (match wasm
    (($ <wasm> types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (define (make-namer names)
       (define namev (list->vector names))
       (lambda (name)
         (if (symbol? name)
             name
             (vector-ref namev name))))
     (define (make-namer/imports kind names)
       (define imported
         (filter-map (match-lambda
                      (($ <import> mod name kind* id type)
                       (and (eq? kind kind*)
                            (or id (error "unexpected id=#f")))))
                     imports))
       (make-namer (append imported names)))

     (define type-name
       (make-namer
        (append-map (match-lambda
                     (($ <rec-group> (($ <type> id) ...)) id)
                     (($ <type> id) (list id)))
                    types)))
     (define func-name
       (make-namer/imports 'func (match funcs ((($ <func> id) ...) id))))
     (define table-name
       (make-namer/imports 'table (match tables ((($ <table> id) ...) id))))
     (define memory-name
       (make-namer/imports 'memory (match memories ((($ <memory> id) ...) id))))
     (define global-name
       (make-namer/imports 'global (match globals ((($ <global> id) ...) id))))
     (define elem-name
       (make-namer (match elems ((($ <elem> id) ...) id))))
     (define data-name
       (make-namer (match datas ((($ <data> id) ...) id))))
     (define tag-name
       (make-namer (match tags ((($ <tag> id) ...) id))))

     (define (struct-field-name struct-type field)
       ;; FIXME: Unimplemented.
       field)

     (define (visit-heap-type type)
       (type-name type))
     (define (visit-val-type type)
       (match type
         (($ <ref-type> nullable? ht)
          (make-ref-type nullable? (visit-heap-type ht)))
         (_ type)))
     (define (visit-ref-type type)
       (visit-val-type type))
     (define (visit-param param)
       (match param
         (($ <param> id type)
          (make-param id (visit-val-type type)))))
     (define (visit-field field)
       (match field
         (($ <field> id mutable? type)
          (make-field id mutable? (visit-val-type type)))))
     (define (visit-func-sig type)
       (match type
         (($ <func-sig> params results)
          (make-func-sig (map visit-param params)
                         (map visit-val-type results)))))
     (define (visit-base-type type)
       (match type
         (($ <struct-type> fields)
          (make-struct-type (map visit-field fields)))
         (($ <array-type> mutable? type)
          (make-array-type mutable? (visit-val-type type)))
         (_
          (visit-func-sig type))))
     (define (visit-sub-type type)
       (match type
         (($ <sub-type> final? supers type)
          (make-sub-type final? (map type-name supers)
                         (visit-base-type type)))
         (_ (visit-base-type type))))
     (define (visit-type-use type)
       (match type
         (($ <type-use> id sig)
          (make-type-use (and=> id type-name)
                         (visit-func-sig sig)))))
     (define (visit-table-type type)
       (match type
         (($ <table-type> limits elem-type)
          (make-table-type limits (visit-val-type elem-type)))))
     (define (visit-global-type type)
       (match type
         (($ <global-type> mutable? type)
          (make-global-type mutable? (visit-val-type type)))))
     (define (visit-block-type type)
       (match type
         (#f #f)
         (($ <type-use>) (visit-type-use type))
         ((or ($ <ref-type>) (? symbol?)) (visit-val-type type))))
     (define (visit-expr* expr local-name)
       (define (visit-expr expr labels)
         (define (label-name label)
           ;; FIXME: We don't yet apply symbolic names to labels.
           label)
         (define visit-inst
           (match-lambda
            (((and inst (or 'block 'loop)) label type body)
             (let* ((labels (cons label labels)))
               `(,inst ,label ,(visit-block-type type)
                       ,(visit-expr body labels))))
            (('if label type consequent alternate)
             (let ((labels (cons label labels)))
               `(if ,label ,(visit-block-type type)
                    ,(visit-expr consequent labels)
                    ,(visit-expr alternate labels))))
            (('try label type body catches catch-all)
             (let ((labels (cons label labels)))
               `(try ,label ,(visit-block-type type)
                     ,(visit-expr body labels)
                     ,(map (lambda (body)
                             (visit-expr body labels))
                           catches)
                     ,(and catch-all
                           (visit-expr catch-all labels)))))
            (('try_delegate label type body handler)
             (let ((labels (cons label labels)))
               `(try_delegate ,label ,(visit-block-type type)
                              ,(visit-expr body labels)
                              ,(label-name handler))))
            (((and inst (or 'throw 'rethrow)) tag) `(,inst ,(tag-name tag)))
            (((and inst (or 'br 'br_if)) label)
             `(,inst ,(label-name label)))
            (('br_table targets default)
             `(br_table ,(map label-name targets) ,(label-name default)))
            (((and inst (or 'call 'return_call)) label)
             `(,inst ,(func-name label)))
            (('call_indirect table type)
             `(call_indirect ,(table-name table) ,(visit-type-use type)))
            (((and inst (or 'call_ref 'return_call_ref)) type)
             `(,inst ,(type-name type)))
            (('select types) `(select ,(map visit-val-type types)))
            (((and inst (or 'local.get 'local.set 'local.tee)) local)
             `(,inst ,(local-name local)))
            (((and inst (or 'global.get 'global.set)) global)
             `(,inst ,(global-name global)))
            (((and inst (or 'table.get 'table.set)) table)
             `(,inst ,(table-name table)))
            (((and inst (or 'memory.size 'memory.grow)) mem)
             `(,inst ,(memory-name mem)))
            (('ref.null ht) `(ref.null ,(visit-heap-type ht)))
            (('ref.func f) `(ref.func ,(func-name f)))

            ;; GC instructions.
            (('ref.null ht)
             `(ref.null ,(visit-heap-type ht)))
            (((and inst (or 'struct.new 'struct.new_default)) type)
             `(,inst ,(type-name type)))
            (((and inst (or 'struct.get 'struct.get_s 'struct.get_u 'struct.set))
              type field)
             `(,inst ,(type-name type) ,(struct-field-name type field)))
            (((and inst (or 'array.new 'array.new_default)) type)
             `(,inst ,(type-name type)))
            (('array.new_fixed type len)
             `(array.new_fixed ,(type-name type) ,len))
            (((and inst (or 'array.new_data 'array.init_data)) type data)
             `(,inst ,(type-name type) ,(data-name data)))
            (((and inst (or 'array.new_elem 'array.init_elem)) type elem)
             `(minst ,(type-name type) ,(elem-name elem)))
            (((and inst (or 'array.get 'array.get_s 'array.get_u 'array.set)) type)
             `(,inst ,(type-name type)))
            (('array.copy dst src)
             `(array.copy ,(type-name dst) ,(type-name src)))
            (((and inst (or 'ref.cast 'ref.test)) rt)
             `(,inst ,(visit-ref-type rt)))
            (((and inst (or 'br_on_cast 'br_on_cast_fail)) label rt1 rt2)
             `(,inst ,(label-name label)
                     ,(visit-ref-type rt1) ,(visit-ref-type rt2)))

            ;; Stringref instructions.
            (('string.const str)
             `(string.const ,(if (string? str) str (list-ref strings str))))
            (((and inst (or 'string.new_utf8 'string.new_lossy_utf8 'string.new_wtf8
                            'string.new_wtf16
                            'string.encode_utf8 'string.encode_lossy_utf8
                            'string.encode_wtf8 'string.encode_wtf16
                            'stringview_wtf8.encode_utf8
                            'stringview_wtf8.encode_lossy_utf8
                            'stringview_wtf8.encode_wtf8
                            'stringview_wtf16.encode))
              mem)
             `(,inst ,(memory-name mem)))

            ;; Misc instructions.
            (('memory.init data mem)
             `(memory.init ,(data-name data) ,(memory-name mem)))
            (('data.drop data)
             `(data.drop ,(data-name data)))
            (('memory.copy dst src)
             `(memory.copy ,(memory-name dst) ,(memory-name src)))
            (('memory.fill mem)
             `(memory.fill ,(memory-name mem)))
            (('table.init elem table)
             `(table.init ,(elem-name elem) ,(table-name table)))
            (('elem.drop elem)
             `(elem.drop ,(elem-name elem)))
            (('table.copy dst src)
             `(table.copy ,(table-name dst) ,(table-name src)))
            (((and inst (or 'table.grow 'table.size 'table.fill)) table)
             `(,inst ,(table-name table)))

            ;; Not yet implemented: simd mem ops, atomic mem ops.

            (inst inst)))
         (map visit-inst expr))
       (visit-expr expr '()))
     (define (visit-init expr)
       (visit-expr* expr error))
     (define (visit-func func)
       (match func
         (($ <func> fid ftype locals body)
          (define local-name
            (match ftype
              (($ <type-use> _ ($ <func-sig> (($ <param> id _) ...) (_ ...)))
               (make-namer
                (append id
                        (match locals ((($ <local> id _) ...) id)))))))
          (let ((type (visit-type-use ftype))
                (body (visit-expr* body local-name)))
            (make-func fid ftype locals body)))))

     (let ((types (map (match-lambda
                        (($ <rec-group> (($ <type> id type) ...))
                         (make-rec-group
                          (map make-type id (map visit-sub-type type))))
                        (($ <type> id type)
                         (make-type (type-name id)
                                    (visit-sub-type type))))
                       types))
           (imports (map (match-lambda
                          (($ <import> mod name kind id type)
                           (let ((type (match kind
                                         ('func (visit-type-use type))
                                         ('table (visit-table-type type))
                                         ('memory type)
                                         ('global (visit-global-type type)))))
                             (make-import mod name kind id type))))
                         imports))
           (funcs (map visit-func funcs))
           (tables (map (match-lambda
                         (($ <table> id type init)
                          (make-table id (visit-table-type type)
                                      (and init (visit-init init)))))
                        tables))
           (globals (map (match-lambda
                          (($ <global> id ($ <global-type> mutable? vt) init)
                           (let* ((vt (visit-val-type vt))
                                  (type (make-global-type mutable? vt)))
                             (make-global id type (visit-init init)))))
                         globals))
           (exports (map (match-lambda
                          (($ <export> name kind idx)
                           (make-export name kind
                                        (match kind
                                          ('func (func-name idx))
                                          ('table (table-name idx))
                                          ('memory (memory-name idx))
                                          ('global (global-name idx))))))
                         exports))
           (start (and=> start func-name))
           (elems (map (match-lambda
                        (($ <elem> id mode table type offset inits)
                         (make-elem id mode (and=> table table-name)
                                    (visit-val-type type)
                                    (and=> offset visit-init)
                                    (map visit-init inits))))
                       elems))
           (datas (map (match-lambda
                        (($ <data> id mode mem offset init)
                         (make-data id mode (and=> mem memory-name)
                                    (and=> offset visit-init)
                                    init)))
                       datas))
           (tags (map (match-lambda
                       (($ <tag> id type)
                        (make-tag (tag-name id) (visit-type-use type))))
                      tags)))
       (make-wasm types imports funcs tables memories globals exports start
                  elems datas tags '() custom)))))

(define (symbolify-wasm wasm)
  (symbolify-uses (symbolify-defs wasm)))
