;;; WebAssembly linker
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
;;; Linker for WebAssembly, to augment a wasm module by pulling in
;;; missing definitions from a standard library.
;;;
;;; Code:

(define-module (wasm link)
  #:use-module (ice-9 match)
  #:use-module (wasm types)
  #:export (add-stdlib))

(define (fold1 f l s0)
  (let lp ((l l) (s0 s0))
    (match l
      (() s0)
      ((elt . l) (lp l (f elt s0))))))

(define (add-stdlib wasm stdlib)
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
  (match stdlib
    (($ <wasm> std-types std-imports std-funcs std-tables std-memories
        std-globals std-exports std-start std-elems std-datas std-tags
        std-strings std-custom)
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
              (fold1 (lambda (param types)
                       (match param
                         (($ <param> id type)
                          (visit-val-type type types))))
                     params
                     (fold1 visit-val-type results types)))))
         (define (visit-sub type types)
           (match type
             (($ <sub-type> supers type)
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
              (#f (match (lookup-type type std-types)
                    (#f (error "unknown heap type" type))
                    (type (visit-type type (cons type types)))))
              (type types)))))
       (define (visit-func-type type types)
         (visit-heap-type type types))
       (define (visit-type-use type types)
         (match type
           (($ <type-use> idx sig)
            ;; recurse into sig?
            (if (symbol? idx)
                (visit-func-type idx types)
                types))))
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
              (((or 'array.new_data 'array.new_elem) type _)
               (visit-heap-type type types))
              (((or 'ref.test 'ref.cast) nullable? type)
               (visit-heap-type type types))
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
       (reverse
        (fold1 visit-function funcs
               (fold1 visit-import imports
                      (fold1 visit-table tables
                             (fold1 visit-global globals
                                    (fold1 visit-elem elems
                                           (fold1 visit-tag tags
                                                  (fold1 visit-type types
                                                         (reverse types))))))))))

     (define (compute-imports imports funcs tables globals exports elems)
       (define (function-locally-bound? label)
         (or-map (match-lambda (($ <func> id) (eqv? label id)))
                 funcs))
       (define (global-locally-bound? label)
         (or-map (match-lambda (($ <global> id type init) (eq? id label)))
                 globals))
       (define (table-locally-bound? label)
         (or-map (match-lambda (($ <table> id type init) (eq? id label)))
                 tables))
       (define (add-import import kind imports)
         (define (lookup name imports)
           (simple-lookup
            imports
            (($ <import> mod' name' kind' id')
             (and (eq? kind' kind) (eqv? id' name)))))
         (match (lookup import imports)
           (#f (match (lookup import std-imports)
                 (#f (error "unknown import" import))
                 (import (cons import imports))))
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
              ('memory imports)))))
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
           (#f (match (lookup name std-funcs)
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
           (#f (match (lookup table std-tables)
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

     (define (compute-globals funcs tables globals exports elems)
       (define (add-global global globals)
         (define (lookup name globals)
           (simple-lookup
            globals
            (($ <global> id) (eqv? id name))))
         (match (lookup global globals)
           (#f (match (lookup global std-globals)
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

     (match wasm
       (($ <wasm> types imports funcs tables memories globals exports
           start elems datas tags strings custom)
        ;; An export can pull in funcs, tables, and globals, possibly imported.
        ;;
        ;; A function can pull in types, funcs, tables, and globals from
        ;; the stdlib.  These fragments may be locally defined or
        ;; imported (except for types which are always locally defined).
        ;;
        ;; A table can pull in types, globals, and functions, possibly
        ;; imported.
        ;;
        ;; A global can pull in types, globals, and functions, possibly imported.
        ;;
        ;; An elem can pull in types and globals, possibly imported.
        ;;
        ;; An import can pull in types.
        ;;
        ;; A tag can pull in types.
        ;;
        ;; A type can pull in other types.
        ;;
        ;; Therefore, to allow pieces of the stdlib to lazily pull in
        ;; other pieces of the stdlib, we do a fixed-point on the set of
        ;; funcs, tables, and globals, then we compute imports and
        ;; types.
        (let fixpoint ((funcs funcs) (tables tables) (globals globals))
          (let* ((funcs' (compute-funcs funcs tables globals exports elems))
                 (tables' (compute-tables funcs' tables exports))
                 (globals' (compute-globals funcs' tables' globals exports elems)))
            (if (and (= (length funcs') (length funcs))
                     (= (length tables') (length tables))
                     (= (length globals') (length globals)))
                (let ((imports (compute-imports imports funcs tables globals
                                                exports elems))
                      (types (compute-types types imports funcs tables globals
                                            elems tags)))
                  (make-wasm types imports funcs tables memories globals exports
                             start elems datas tags strings custom))
                (fixpoint funcs' tables' globals')))))))))
