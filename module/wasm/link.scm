;;; WebAssembly linker
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
;;; Linker for WebAssembly, to augment a wasm module by pulling in
;;; missing definitions from a standard library.
;;;
;;; Code:

(define-module (wasm link)
  #:use-module (ice-9 match)
  #:use-module (wasm types)
  #:use-module (wasm types)
  #:export (add-stdlib))

(define (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define (sort-types types)
  (define visited (make-hash-table))
  (define (visited? type) (hashq-ref visited type))
  (define (mark-visited! type) (hashq-set! visited type #t))
  (define (lookup-type name)
    ;; Return the whole type block, so we can revisit any
    ;; references within it.
    (or-map (lambda (type)
              (match type
                (($ <type> id _) (and (eq? id name) type))
                (($ <rec-group> (($ <type> id) ...))
                 (and (or-map (lambda (id) (eq? id name)) id)
                      type))))
            types))
  (define (visit-heap-type type order)
    (match (lookup-type type)
      (#f order)
      (type (visit-type type order))))
  (define (visit-val-type type order)
    (match type
      (($ <ref-type> nullable? ht)
       (visit-heap-type ht order))
      (_ order)))
  (define (visit-storage-type type order)
    (visit-val-type type order))
  (define (visit-successors type order)
    (define (visit-base type order)
      (match type
        (($ <array-type> mutable? type)
         (visit-storage-type type order))
        (($ <struct-type> fields)
         (fold1 (lambda (field order)
                  (match field
                    (($ <field> id mutable? type)
                     (visit-storage-type type order))))
                fields order))
        (($ <func-sig> params results)
         (fold1 (lambda (param order)
                  (match param
                    (($ <param> id type)
                     (visit-val-type type order))))
                params (fold1 visit-val-type results order)))))
    (define (visit-sub type order)
      (match type
        (($ <sub-type> final? supers type)
         (visit-base type (fold1 visit-heap-type supers order)))
        (_ (visit-base type order))))
    (match type
      (($ <rec-group> (($ <type> id type) ...))
       (fold1 visit-sub type order))
      (($ <type> id type)
       (visit-sub type order))))
  (define (visit-type type order)
    (cond
     ((visited? type) order)
     (else
      ;; After visiting successors, add label to the reverse post-order.
      (mark-visited! type)
      (cons type (visit-successors type order)))))
  (reverse (fold1 visit-type types '())))

(define* (link wasm #:key
               (link-type (lambda (id) #f))
               (link-import (lambda (id kind) #f))
               (link-func (lambda (id) #f))
               (link-table (lambda (id) #f))
               (link-memory (lambda (id) #f))
               (link-global (lambda (id) #f))
               (link-data (lambda (id) #f)))
  (define (fold-instructions f body seed)
    (define (visit* body seed)
      (fold1 visit1 body seed))
    (define (visit1 inst seed)
      (let ((seed (f inst seed)))
        (match inst
          (((or 'block 'loop) label type insts)
           (visit* insts seed))
          (('if label type consequent alternate)
           (visit* alternate (visit* consequent seed)))
          (('try label type body catches catch-all)
           (let ((seed (if catch-all (visit* catch-all seed) seed)))
             (fold1 visit* catches (visit* body seed))))
          (('try_delegate label type body handler)
           (visit* body seed))
          (_ seed))))
    (visit* body seed))
  (define-syntax-rule (simple-lookup candidates (pat test) ...)
    (let lp ((candidates candidates))
      (match candidates
        (() #f)
        (((and candidate pat) . candidates)
         (if test candidate (lp candidates)))
        ...)))
  (define (compute-types types imports funcs tables globals elems tags)
    (define (lookup-type name types)
      ;; Return the whole type block, so we can revisit any
      ;; references within it.
      (or-map (lambda (type)
                (match type
                  (($ <type> id _) (and (eq? id name) type))
                  (($ <rec-group> (($ <type> id) ...))
                   (and (or-map (lambda (id) (eq? id name)) id)
                        type))))
              types))
    (define (visit-val-type type types)
      (match type
        ((or 'i32 'i64 'f32 'f64 'v128
             'funcref 'externref 'anyref 'eqref 'i31ref
             'nullexternref 'nullfuncref
             'structref 'arrayref
             'nullref
             'stringref
             'stringview_wtf8ref 'stringview_wtf16ref 'stringview_iterref)
         types)
        (($ <ref-type> nullable? ht)
         (visit-heap-type ht types))))
    (define (visit-storage-type type types)
      (match type
        ((or 'i8 'i16) types)
        (_ (visit-val-type type types))))
    (define (visit-func-sig params results types)
      (fold1 (lambda (param types)
               (match param
                 (($ <param> id type)
                  (visit-val-type type types))))
             params
             (fold1 visit-val-type results types)))
    (define (visit-type type types)
      (define (visit-base type types)
        (match type
          (($ <array-type> mutable? type)
           (visit-storage-type type types))
          (($ <struct-type> fields)
           (fold1 (lambda (field types)
                    (match field
                      (($ <field> id mutable? type)
                       (visit-storage-type type types))))
                  fields types))
          (($ <func-sig> params results)
           (visit-func-sig params results types))))
      (define (visit-sub type types)
        (match type
          (($ <sub-type> final? supers type)
           (visit-base type
                       (fold1 visit-heap-type supers types)))
          (_ (visit-base type types))))
      (match type
        (($ <rec-group> (($ <type> id type) ...))
         (fold1 visit-sub type types))
        (($ <type> id type)
         (visit-sub type types))))
    (define (visit-heap-type type types)
      (match type
        ((or 'func 'extern 'any 'eq 'i31 'noextern 'nofunc 'struct 'array 'none
             'string 'stringview_wtf8 'stringview_wtf16 'stringview_iter)
         types)
        (_
         (match (lookup-type type types)
           (#f (let ((type (or (link-type type)
                               (error "unknown heap type" type))))
                 (visit-type type (cons type types))))
           (type types)))))
    (define (visit-ref-type type types)
      (match type
        (($ <ref-type> nullable? ht)
         (visit-heap-type ht types))
        (_ types)))
    (define (visit-func-type type types)
      (visit-heap-type type types))
    (define (visit-type-use type types)
      (match type
        (($ <type-use> idx ($ <func-sig> params results))
         (let ((types (visit-func-sig params results types)))
           (if (symbol? idx)
               (visit-func-type idx types)
               types)))))
    (define (visit-body body types)
      (fold-instructions
       (lambda (inst types)
         (match inst
           (((or 'block 'loop 'if 'try 'try_delegate) label type . _)
            (if type
                (visit-type-use type types)
                types))
           (((or 'call_indirect 'return_call_indirect) table type)
            (visit-type-use type types))
           (((or 'call_ref 'return_call_ref) table type)
            (visit-type-use type types))
           (('select type ...)
            (fold1 visit-val-type type types))
           (('ref.null type)
            (visit-heap-type type types))
           (((or 'struct.get 'struct.get_s 'struct.get_u
                 'struct.set) type field)
            (visit-heap-type type types))
           (((or 'struct.new 'struct.new_default
                 'array.new 'array.new_default
                 'array.get 'array.get_s 'array.get_u
                 'array.set) type)
            (visit-heap-type type types))
           (('array.copy dst src)
            (visit-heap-type dst (visit-heap-type src types)))
           (((or 'array.new_fixed 'array.new_data 'array.new_elem
                 'array.init_data 'array.init_elem) type _)
            (visit-heap-type type types))
           (((or 'ref.test 'ref.cast) ($ <ref-type> nullable? type))
            (visit-heap-type type types))
           (((or 'br_on_cast 'br_on_cast_fail) idx rt1 rt2)
            (visit-ref-type rt1 (visit-ref-type rt2 types)))
           (_ types)))
       body types))
    (define (visit-function func types)
      (match func
        (($ <func> id type (($ <local> lid ltype) ...) body)
         (visit-body
          body
          (fold1 visit-val-type ltype
                 (visit-type-use type types))))))
    (define (visit-import import types)
      (match import
        (($ <import> mod name 'func id type)
         (visit-type-use type types))
        (($ <import> mod name 'table id ($ <table-type> limits type))
         (visit-val-type type types))
        (($ <import> mod name 'memory id type)
         types)
        (($ <import> mod name 'global id ($ <global-type> mutable? type))
         (visit-val-type type types)))
      types)
    (define (visit-table table types)
      (match table
        (($ <table> id ($ <table-type> limits type) init)
         (visit-val-type type
                         (if init
                             (visit-body init types)
                             types)))))
    (define (visit-global global types)
      (match global
        (($ <global> id ($ <global-type> mutable? type) init)
         (visit-val-type type (visit-body init types)))))
    (define (visit-elem elem types)
      (match elem
        (($ <elem> id mode table type offset inits)
         (let* ((types (fold1 visit-body inits types))
                (types (visit-val-type type types)))
           (if offset
               (visit-body offset types)
               types)))))
    (define (visit-tag tag types)
      (match tag
        (($ <tag> id type)
         (visit-type-use type types))))
    (sort-types
     (fold1 visit-function funcs
            (fold1 visit-import imports
                   (fold1 visit-table tables
                          (fold1 visit-global globals
                                 (fold1 visit-elem elems
                                        (fold1 visit-tag tags
                                               (fold1 visit-type types
                                                      types)))))))))

  (define (compute-imports imports funcs tables memories globals exports
                           elems)
    (define (function-locally-bound? label)
      (or-map (match-lambda (($ <func> id) (eqv? label id)))
              funcs))
    (define (global-locally-bound? label)
      (or-map (match-lambda (($ <global> id type init) (eq? id label)))
              globals))
    (define (table-locally-bound? label)
      (or-map (match-lambda (($ <table> id type init) (eq? id label)))
              tables))
    (define (memory-locally-bound? label)
      (or-map (match-lambda (($ <memory> id type) (eq? id label)))
              memories))
    (define (add-import import kind imports)
      (define (lookup name imports)
        (simple-lookup
         imports
         (($ <import> mod' name' kind' id')
          (and (eq? kind' kind) (eqv? id' name)))))
      (match (lookup import imports)
        (#f (cons (or (link-import import kind)
                      (error "unknown import" import kind))
                  imports))
        (_ imports)))
    (define (add-imported-func label imports)
      (if (function-locally-bound? label)
          imports
          (add-import label 'func imports)))
    (define (add-imported-table label imports)
      (if (table-locally-bound? label)
          imports
          (add-import label 'table imports)))
    (define (add-imported-global label imports)
      (if (global-locally-bound? label)
          imports
          (add-import label 'global imports)))
    (define (add-imported-memory label imports)
      (if (memory-locally-bound? label)
          imports
          (add-import label 'memory imports)))
    (define (visit-body body imports)
      (fold-instructions
       (lambda (inst imports)
         (match inst
           (((or 'call 'return_call 'ref.func) label)
            (add-imported-func label imports))
           (((or 'table.get 'table.set
                 'table.grow 'table.size 'table.fill) label)
            (add-imported-table label imports))
           (('table.init elem table)
            (add-imported-table table imports))
           (('call_indirect type table)
            (add-imported-table table imports))
           (('table.copy dst src)
            (add-imported-table dst (add-imported-table src imports)))
           (((or 'global.get 'global.set) label)
            (add-imported-global label imports))
           (((or 'i32.load 'i64.load 'f32.load 'f64.load
                 'i32.load8_s 'i32.load8_u 'i32.load16_s 'i32.load16_u
                 'i64.load8_s 'i64.load8_u 'i64.load16_s 'i64.load16_u
                 'i64.load32_s 'i64.load32_u
                 'i32.store 'i64.store 'f32.store 'f64.store
                 'i32.store8 'i32.store16 'i64.store8 'i64.store16
                 'i64.store32)
             ($ <mem-arg> id offset align))
            (add-imported-memory id imports))
           (((or 'memory.size 'memory.grow 'memory.init 'memory.fill) id)
            (add-imported-memory id imports))
           (('memory.copy dst src)
            (add-imported-memory dst (add-imported-memory src imports)))
           (_ imports)))
       body imports))
    (define (visit-func func imports)
      (match func
        (($ <func> id type locals body)
         (visit-body body imports))))
    (define (visit-table table imports)
      (match table
        (($ <table> id type init)
         (if init
             (visit-body init imports)
             imports))))
    (define (visit-global global imports)
      (match global
        (($ <global> id type init)
         (visit-body init imports))))
    (define (visit-export export imports)
      (match export
        (($ <export> name kind id)
         (match kind
           ('func (add-imported-func id imports))
           ('table (add-imported-table id imports))
           ('global (add-imported-global id imports))
           ('memory (add-imported-memory id imports))))))
    (define (visit-elem elem imports)
      (match elem
        (($ <elem> id mode table type offset inits)
         (let ((imports (fold1 visit-body inits imports)))
           (if offset
               (visit-body offset imports)
               imports)))))
    (reverse
     (fold1 visit-func funcs
            (fold1 visit-table tables
                   (fold1 visit-global globals
                          (fold1 visit-export exports
                                 (fold1 visit-elem elems
                                        (reverse imports))))))))

  (define (compute-funcs funcs tables globals exports elems)
    (define (add-func name funcs)
      (define (lookup name funcs)
        (simple-lookup funcs (($ <func> id) (eqv? id name))))
      (match (lookup name funcs)
        (#f (match (link-func name)
              (#f funcs)
              (func (visit-func func (cons func funcs)))))
        (_ funcs)))
    (define (visit-body body funcs)
      (fold-instructions
       (lambda (inst funcs)
         (match inst
           (((or 'call 'return_call 'ref.func) f)
            (add-func f funcs))
           (_ funcs)))
       body funcs))
    (define (visit-func func funcs)
      (match func
        (($ <func> id type locals body)
         (visit-body body funcs))))
    (define (visit-table table funcs)
      (match table
        (($ <table> id type init)
         (if init
             (visit-body init funcs)
             funcs))))
    (define (visit-global global funcs)
      (match global
        (($ <global> id type init)
         (visit-body init funcs))))
    (define (visit-export export funcs)
      (match export
        (($ <export> name kind id)
         (if (eq? kind 'func)
             (add-func id funcs)
             funcs))))
    (define (visit-elem elem funcs)
      (match elem
        (($ <elem> id mode table type offset inits)
         (let ((funcs (fold1 visit-body inits funcs)))
           (if offset
               (visit-body offset funcs)
               funcs)))))
    (reverse
     (fold1 visit-func funcs
            (fold1 visit-table tables
                   (fold1 visit-global globals
                          (fold1 visit-export exports
                                 (fold1 visit-elem elems
                                        (reverse funcs))))))))

  (define (compute-tables funcs tables exports)
    (define (add-table table tables)
      (define (lookup name tables)
        (simple-lookup
         tables
         (($ <table> id) (eqv? id name))))
      (match (lookup table tables)
        (#f (match (link-table table)
              (#f tables)
              (table (cons table tables))))
        (_ tables)))
    (define (visit-func func tables)
      (match func
        (($ <func> id type locals body)
         (fold-instructions
          (lambda (inst tables)
            (match inst
              (((or 'table.get 'table.set
                    'table.grow 'table.size 'table.fill)
                table)
               (add-table table tables))
              (('table.init elem table)
               (add-table table tables))
              (('table.copy dst src)
               (add-table dst (add-table src tables)))
              (('call_indirect table type)
               (add-table table tables))
              (_ tables)))
          body tables))))
    (define (visit-export export tables)
      (match export
        (($ <export> name kind id)
         (if (eq? kind 'table)
             (add-table id tables)
             tables))))
    (reverse (fold1 visit-func funcs
                    (fold1 visit-export exports (reverse tables)))))

  (define (compute-memories funcs memories exports datas)
    (define (add-memory memory memories)
      (define (lookup name memories)
        (simple-lookup
         memories
         (($ <memory> id) (eqv? id name))))
      (match (lookup memory memories)
        (#f (match (link-memory memory)
              (#f memories)
              (memory (cons memory memories))))
        (_ memories)))
    (define (visit-body body memories)
      (fold-instructions
       (lambda (inst memories)
         (match inst
           (((or 'i32.load 'i64.load 'f32.load 'f64.load
                 'i32.load8_s 'i32.load8_u 'i32.load16_s 'i32.load16_u
                 'i64.load8_s 'i64.load8_u 'i64.load16_s 'i64.load16_u
                 'i64.load32_s 'i64.load32_u
                 'i32.store 'i64.store 'f32.store 'f64.store
                 'i32.store8 'i32.store16 'i64.store8 'i64.store16
                 'i64.store32)
             ($ <mem-arg> id offset align))
            (add-memory id memories))
           (((or 'memory.size 'memory.grow 'memory.init 'memory.fill) id)
            (add-memory id memories))
           (('memory.copy dst src)
            (add-memory dst (add-memory src memories)))
           (_ memories)))
       body memories))
    (define (visit-func func memories)
      (match func
        (($ <func> id type locals body)
         (visit-body body memories))))
    (define (visit-export export memories)
      (match export
        (($ <export> name kind id)
         (if (eq? kind 'memory)
             (add-memory id memories)
             memories))))
    (define (visit-data data memories)
      (match data
        (($ <data> id mode mem offset init)
         (if (eq? mode 'active)
             (add-memory mem memories)
             memories))))
    (reverse
     (fold1 visit-func funcs
            (fold1 visit-export exports
                   (fold1 visit-data datas
                          (reverse memories))))))

  (define (compute-globals funcs tables globals exports elems)
    (define (add-global global globals)
      (define (lookup name globals)
        (simple-lookup
         globals
         (($ <global> id) (eqv? id name))))
      (match (lookup global globals)
        (#f (match (link-global global)
              (#f globals)
              (global (visit-global global (cons global globals)))))
        (_ globals)))
    (define (visit-body body globals)
      (fold-instructions
       (lambda (inst globals)
         (match inst
           (((or 'global.get 'global.set) global)
            (add-global global globals))
           (_ globals)))
       body globals))
    (define (visit-func func globals)
      (match func
        (($ <func> id type locals body)
         (visit-body body globals))))
    (define (visit-table table globals)
      (match table
        (($ <table> id type init)
         (if init
             (visit-body init globals)
             globals))))
    (define (visit-global global globals)
      (match global
        (($ <global> id type init)
         (visit-body init globals))))
    (define (visit-export export globals)
      (match export
        (($ <export> name kind id)
         (if (eq? kind 'global)
             (add-global id globals)
             globals))))
    (define (visit-elem elem globals)
      (match elem
        (($ <elem> id mode table type offset inits)
         (let ((globals (fold1 visit-body inits globals)))
           (if offset
               (visit-body offset globals)
               globals)))))
    (reverse
     (fold1 visit-func funcs
            (fold1 visit-table tables
                   (fold1 visit-global globals
                          (fold1 visit-export exports
                                 (fold1 visit-elem elems
                                        (reverse globals))))))))

  (define (compute-datas funcs tables globals datas)
    (define (add-data data datas)
      (define (lookup name datas)
        (simple-lookup
         datas
         (($ <data> id) (eqv? id name))))
      (match (lookup data datas)
        (#f (match (link-data data)
              (#f datas)
              (data (cons data datas))))
        (_ datas)))
    (define (visit-body body datas)
      (fold-instructions
       (lambda (inst datas)
         (match inst
           (((or 'array.new_data 'array.init_data) type data)
            (add-data data datas))
           (_ datas)))
       body datas))
    (define (visit-func func datas)
      (match func
        (($ <func> id type locals body)
         (visit-body body datas))))
    (define (visit-table table datas)
      (match table
        (($ <table> id type init)
         (if init
             (visit-body init datas)
             datas))))
    (define (visit-global global datas)
      (match global
        (($ <global> id type init)
         (visit-body init datas))))
    (reverse
     (fold1 visit-func funcs
            (fold1 visit-table tables
                   (fold1 visit-global globals
                          (reverse datas))))))

  (match wasm
    (($ <wasm> types imports funcs tables memories globals exports
        start elems datas tags strings custom)
     ;; An export can pull in funcs, tables, globals, and memories,
     ;; possibly imported.
     ;;
     ;; A function can pull in types, funcs, tables, globals and
     ;; memories from the stdlib.  These fragments may be locally
     ;; defined or imported (except for types which are always
     ;; locally defined).
     ;;
     ;; A table can pull in types, globals, and functions, possibly
     ;; imported.
     ;;
     ;; A global can pull in types, globals, and functions, possibly
     ;; imported.
     ;;
     ;; An elem can pull in types and globals, possibly imported.
     ;;
     ;; An import can pull in types.
     ;;
     ;; A tag can pull in types.
     ;;
     ;; A type can pull in other types.
     ;;
     ;; Data can pull in a memory.
     ;;
     ;; Memories can't pull in anything else.
     ;;
     ;; Therefore, to allow pieces of the stdlib to lazily pull in
     ;; other pieces of the stdlib, we do a fixed-point on the set of
     ;; funcs, tables, and globals, then we compute memories, imports
     ;; and types.
     (let fixpoint ((funcs funcs) (tables tables) (globals globals))
       (let* ((funcs' (compute-funcs funcs tables globals exports elems))
              (tables' (compute-tables funcs' tables exports))
              (globals' (compute-globals funcs' tables' globals exports elems)))
         (if (and (= (length funcs') (length funcs))
                  (= (length tables') (length tables))
                  (= (length globals') (length globals)))
             (let* ((datas (compute-datas funcs tables globals datas))
                    (memories (compute-memories funcs memories exports datas)))
               (let ((imports (compute-imports imports funcs tables memories
                                               globals exports elems))
                     (types (compute-types types imports funcs tables globals
                                           elems tags)))
                 (make-wasm types imports funcs tables memories globals exports
                            start elems datas tags strings custom)))
             (fixpoint funcs' tables' globals')))))))

(define* (add-stdlib wasm stdlib #:key
                     (synthesize-type (lambda (id) #f))
                     (synthesize-import (lambda (id kind) #f)))
  (match stdlib
    (($ <wasm> std-types std-imports std-funcs std-tables std-memories
        std-globals std-exports std-start std-elems std-datas std-tags
        std-strings std-custom)
     (define types (make-hash-table))
     (define imports (make-hash-table))
     (define funcs (make-hash-table))
     (define tables (make-hash-table))
     (define memories (make-hash-table))
     (define globals (make-hash-table))
     (define datas (make-hash-table))

     (for-each (match-lambda
                 ((and t ($ <type> id _)) (hashq-set! types id t))
                 ((and t ($ <rec-group> (($ <type> id) ...)))
                  (for-each (lambda (id) (hashq-set! types id t)) id)))
               std-types)
     (for-each (match-lambda
                 ((and import ($ <import> mode name kind id type))
                  (hash-set! imports (cons id kind) import)))
               std-imports)
     (for-each (match-lambda
                 ((and func ($ <func> id type locals body))
                  (hashq-set! funcs id func)))
               std-funcs)
     (for-each (match-lambda
                 ((and table ($ <table> id type init))
                  (hashq-set! tables id table)))
               std-tables)
     (for-each (match-lambda
                 ((and memory ($ <memory> id type))
                  (hashq-set! memories id memory)))
               std-memories)
     (for-each (match-lambda
                 ((and global ($ <global> id type init))
                  (hashq-set! globals id global)))
               std-globals)
     (for-each (match-lambda
                 ((and data ($ <data> id mode mem offset init))
                  (hashq-set! datas id data)))
               std-datas)

     (link wasm
           #:link-type (lambda (id)
                         (or (hashq-ref types id)
                             (synthesize-type id)))
           #:link-import (lambda (id kind)
                           (or (hash-ref imports (cons id kind))
                               (synthesize-import id kind)))
           #:link-func (lambda (id) (hashq-ref funcs id))
           #:link-table (lambda (id) (hashq-ref tables id))
           #:link-memory (lambda (id) (hashq-ref memories id))
           #:link-global (lambda (id) (hashq-ref globals id))
           #:link-data (lambda (id) (hashq-ref datas id))))))
