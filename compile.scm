(use-modules (ice-9 binary-ports)
             (ice-9 control)
             (ice-9 format)
             (ice-9 match)
             (ice-9 pretty-print)
             ((srfi srfi-1) #:select (append-map))
             (system base compile)
             (system base language)
             (language cps)
             (language cps intset)
             (language cps intmap)
             (language cps tailify)
             (language cps verify)
             (language cps renumber)
             (language cps dce)
             (language cps simplify)
             (language cps dump)
             (language cps utils)
             (wasm assemble)
             (wasm dump)
             (wasm resolve)
             (wasm parse)
             (wasm types))

(define (invert-tree parents)
  (intmap-fold
   (lambda (child parent tree)
     (let ((tree (intmap-add tree child empty-intset intset-union)))
       (match parent
         (-1 tree)
         (_ (intmap-add tree parent (intset child) intset-union)))))
   parents empty-intmap))

(define (intset-filter pred set)
  (persistent-intset
   (intset-fold (lambda (i out)
                  (if (pred i) (intset-add! out i) out))
                set empty-intset)))

(define (intset-pop set)
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define void-block-type (make-type-use #f (make-func-sig '() '())))

;; Codegen improvements:
;;
;; 1. Eliminating directly-used locals.  Compute a set of variables that
;;    are used once, right after they are defined, in such a way that
;;    they can just flow to their use sites on the stack.  Avoid
;;    creating locals for these.
;;
;; 2. Instruction selection.  Could be that some instructions could be
;;    combined or rescheduled.  Perhaps this is something on the CPS
;;    level though.
;;
;; 3. Optimised local allocation.  Do graph coloring to map variables to
;;    a smaller set of locals, to avoid emitting one local per variable.

(define scm-type (make-ref-type #f 'eq))
(define kvarargs-sig
  (make-func-sig (list (make-param '$nargs 'i32)
                       (make-param '$arg0 scm-type)
                       (make-param '$arg1 scm-type)
                       (make-param '$arg2 scm-type))'()))

(define (compute-stdlib import-abi?)
  (define func-types
    (list (make-type '$kvarargs kvarargs-sig)))

  (define heap-types
    (letrec-syntax
        ((struct-field (syntax-rules (mut)
                         ((_ name (mut type))
                          (make-field 'name #t type))
                         ((_ name type)
                          (make-field 'name #f type))))
         (struct-type (syntax-rules ()
                        ((_ (field spec ...) ...)
                         (make-struct-type (list (struct-field field spec ...) ...)))))
         (struct* (syntax-rules ()
                    ((_ (name) (field spec ...) ...)
                     (make-type 'name
                                (struct-type (field spec ...) ...)))
                    ((_ (name super ...) (field spec ...) ...)
                     (make-type 'name
                                (make-sub-type
                                 '(super ...)
                                 (struct-type (field spec ...) ...))))))
         (struct (syntax-rules ()
                   ((_ (name super ...) (field spec ...) ...)
                    (struct* (name super ...)
                             ($hash (mut 'i32))
                             (field spec ...) ...)))))
      (list
       (make-type '$raw-bitvector (make-array-type #t 'i32))
       (make-type '$raw-bytevector (make-array-type #t 'i8))
       (make-type '$raw-scmvector (make-array-type #t scm-type))
       ;; In theory we could just include those members of the rec group
       ;; that the program needs, but to allow interoperability with
       ;; separately-compiled modules, we'll just put in the whole rec
       ;; group if any member is needed.
       (make-rec-group
        (list
         (struct ($heap-object))
         (struct ($extern-ref $heap-object)
                 ($val (make-ref-type #f 'extern)))
         (struct ($bignum $heap-object)
                 ($val (make-ref-type #f 'extern)))
         (struct ($flonum $heap-object)
                 ($val 'f64))
         (struct ($complex $heap-object)
                 ($real 'f64)
                 ($imag 'f64))
         (struct ($fraction $heap-object)
                 ($num scm-type)
                 ($denom scm-type))
         (struct ($pair $heap-object)
                 ($car (mut scm-type))
                 ($cdr (mut scm-type)))
         (struct ($mutable-pair $pair)
                 ($car (mut scm-type))
                 ($cdr (mut scm-type)))
         (struct ($vector $heap-object)
                 ($vals (make-ref-type #f '$raw-scmvector)))
         (struct ($mutable-vector $vector)
                 ($vals (make-ref-type #f '$raw-scmvector)))
         (struct ($bytevector $heap-object)
                 ($vals (make-ref-type #f '$raw-bytevector)))
         (struct ($mutable-bytevector $bytevector)
                 ($vals (make-ref-type #f '$raw-bytevector)))
         (struct ($bitvector $heap-object)
                 ($len 'i32)
                 ($vals (make-ref-type #f '$raw-bitvector)))
         (struct ($mutable-bitvector $bitvector)
                 ($len 'i32)
                 ($vals (make-ref-type #f '$raw-bitvector)))
         (struct ($string $heap-object)
                 ($str (mut (make-ref-type #f 'string))))
         (struct ($mutable-string $string)
                 ($str (mut (make-ref-type #f 'string))))
         (struct ($proc $heap-object)
                 ($func (make-ref-type #f '$kvarargs)))
         (struct ($symbol $heap-object)
                 ($name (make-ref-type #f '$string)))
         (struct ($keyword $heap-object)
                 ($name (make-ref-type #f '$symbol)))
         (struct ($variable $heap-object)
                 ($val (mut scm-type)))
         (struct ($atomic-box $heap-object)
                 ($val (mut scm-type)))
         (struct ($hash-table $heap-object)
                 ($size (make-ref-type #f 'i31))
                 ($buckets (make-ref-type #f '$vector)))
         (struct ($weak-table $heap-object)
                 ($val (make-ref-type #f 'extern)))
         (struct ($fluid $heap-object)
                 ($init scm-type))
         (struct ($dynamic-state $heap-object)
                 ($val (make-ref-type #f 'extern)))
         (struct ($syntax $heap-object)
                 ($expr scm-type)
                 ($wrap scm-type)
                 ($module scm-type)
                 ($source scm-type))
         (struct* ($port-type)
                  ($name (make-ref-type #f 'string))
                  ;; in guile these are (port, bv, start, count) -> size_t
                  ($read (make-ref-type #t '$proc)) ;; could have a more refined type
                  ($write (make-ref-type #t '$proc))
                  ($seek (make-ref-type #t '$proc)) ;; (port, offset, whence) -> offset
                  ($close (make-ref-type #t '$proc)) ;; (port) -> ()
                  ($get-natural-buffer-sizes (make-ref-type #t '$proc)) ;; port -> (rdsz, wrsz)
                  ($random-access? (make-ref-type #t '$proc)) ;; port -> bool
                  ($input-waiting (make-ref-type #t '$proc)) ;; port -> bool
                  ($truncate (make-ref-type #t '$proc)) ;; (port, length) -> ()
                  ;; Guile also has GOOPS classes here.
                  )
         (struct ($port $heap-object)
                 ($pt (make-ref-type #f '$port-type))
                 ($stream (mut scm-type))
                 ($file_name (mut scm-type))
                 ($position (mut (make-ref-type #f '$mutable-pair)))
                 ($read_buf (mut scm-type))      ;; A 5-vector
                 ($write_buf (mut scm-type))     ;; A 5-vector
                 ($write_buf_aux (mut scm-type)) ;; A 5-vector
                 ($read_buffering (mut 'i32))
                 ($refcount (mut 'i32))
                 ($rw_random (mut 'i8))
                 ($properties (mut scm-type)))
         (struct ($struct $heap-object)
                 ;; Vtable link is mutable so that we can tie the knot for
                 ;; top types.
                 ($vtable (mut (make-ref-type #t '$vtable))))
         (struct ($vtable $struct)
                 ($vtable (mut (make-ref-type #t '$vtable)))
                 ($field0 (mut scm-type))
                 ($field1 (mut scm-type))
                 ($field2 (mut scm-type))
                 ($field3 (mut scm-type))))))))

  (define types (append func-types heap-types))

  (define-syntax imported-function-type
    (syntax-rules (=>)
      ((_ (param-type ...) => (result-type ...))
       (make-type-use #f (make-func-sig (list (make-param #f param-type)
                                              ...)
                                        (list result-type ...))))))
  (define imports
    (list
     (make-import "rt" "get_argument" 'func '$get-argument
                  (imported-function-type ('i32) => (scm-type)))
     (make-import "rt" "prepare_return_values" 'func '$prepare-return-values
                  (imported-function-type ('i32) => ()))
     (make-import "rt" "set_return_value" 'func '$set-return-value
                  (imported-function-type ('i32 scm-type) => ()))
     
     (make-import "rt" "bignum_from_i64" 'func '$bignum-from-i64
                  (imported-function-type ('i64)
                                          => ((make-ref-type #f 'extern))))
     (make-import "rt" "bignum_from_u64" 'func '$bignum-from-u64
                  (imported-function-type ('i64)
                                          => ((make-ref-type #f 'extern))))
     (make-import "rt" "bignum_is_i64" 'func '$bignum-is-i64
                  (imported-function-type ((make-ref-type #f 'extern))
                                          => ('i32)))
     (make-import "rt" "bignum_is_u64" 'func '$bignum-is-u64
                  (imported-function-type ((make-ref-type #f 'extern))
                                          => ('i32)))
     (make-import "rt" "bignum_get_i64" 'func '$bignum-get-i64
                  (imported-function-type ((make-ref-type #f 'extern))
                                          => ('i64)))
     (make-import "rt" "make_weak_map" 'func '$make-weak-map
                  (imported-function-type ()
                                          => ((make-ref-type #f 'extern))))
     (make-import "rt" "weak_map_get" 'func '$weak-map-get
                  (imported-function-type ((make-ref-type #f 'extern))
                                          => (scm-type)))
     (make-import "rt" "weak_map_set" 'func '$weak-map-set
                  (imported-function-type ((make-ref-type #f 'extern)
                                           scm-type scm-type)
                                          => ()))
     (make-import "rt" "weak_map_delete" 'func '$weak-map-delete
                  (imported-function-type ((make-ref-type #f 'extern) scm-type)
                                          => ('i32)))))

  (define funcs
    (list (make-func '$pop-return!
                     (make-type-use #f (make-func-sig
                                        '()
                                        (list (make-ref-type #f '$kvarargs))))
                     '()
                     `((global.get $return-sp)
                       (i32.const 1)
                       (i32.sub)
                       (global.set $return-sp)
                       (global.get $return-sp)
                       (table.get $return-stack)))))

  (define tables
    (list (make-table '$argv
                      (make-table-type
                       (make-limits 0 #f)
                       scm-type)
                      '((i32.const 0) i31.new))
          (make-table '$return-stack
                      (make-table-type
                       (make-limits 0 #f)
                       (make-ref-type #f '$kvarargs))
                      #f)))

  (define memories '())

  (define globals
    (let ((scm-init '((i32.const 0) i31.new)))
      (list (make-global '$arg3 (make-global-type #t scm-type) scm-init)
            (make-global '$arg4 (make-global-type #t scm-type) scm-init)
            (make-global '$arg5 (make-global-type #t scm-type) scm-init)
            (make-global '$arg6 (make-global-type #t scm-type) scm-init)
            (make-global '$arg7 (make-global-type #t scm-type) scm-init)
            (make-global '$return-sp (make-global-type #t 'i32) '((i32.const 0))))))

  (define exports '())
  (define start #f)
  (define elems '())
  (define datas '())
  (define tags '())
  (define strings '())
  (define custom '())

  (make-wasm (append func-types heap-types)
             (if import-abi?
                 (append
                  (map (match-lambda
                        (($ <global> id type init)
                         (make-import "abi" (symbol->string id) 'global id
                                      type)))
                       globals)
                  (map (match-lambda
                        (($ <table> id type init)
                         (make-import "abi" (symbol->string id) 'table id
                                      type)))
                       tables)
                  imports)
                 imports)
             funcs
             (if import-abi? '() tables)
             memories
             (if import-abi? '() globals)
             exports start elems datas tags strings custom))

(define (add-stdlib wasm import-abi?)
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
  (match (compute-stdlib import-abi?)
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
       (define (visit-type type types)
         (define (visit-base type types)
           (match type
             (($ <array-type> mutable? type)
              (visit-val-type type types))
             (($ <struct-type> fields)
              (fold1 (lambda (field)
                       (match field
                         (($ <field> id mutable? type)
                          (visit-val-type type types))))
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

     (define (compute-imports imports funcs tables globals elems)
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
                             (fold1 visit-elem elems
                                    (reverse imports)))))))

     (define (compute-funcs funcs)
       (define (add-func name funcs)
         (define (lookup name funcs)
           (simple-lookup funcs (($ <func> id) (eqv? id name))))
         (match (lookup name funcs)
           (#f (match (lookup name std-funcs)
                 (#f funcs)
                 (func (visit-func func (cons func funcs)))))
           (_ funcs)))
       (define (visit-func func funcs)
         (match func
           (($ <func> id type locals body)
            (fold-instructions
             (lambda (inst funcs)
               (match inst
                 (((or 'call 'return_call 'ref.func) f)
                  (add-func f funcs))
                 (_ funcs)))
             body funcs))))
       (reverse (fold1 visit-func funcs (reverse funcs))))

     (define (compute-tables funcs tables)
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
       (reverse (fold1 visit-func funcs (reverse tables))))

     (define (compute-globals funcs tables globals elems)
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
                             (fold1 visit-elem elems
                                    (reverse globals)))))))

     (match wasm
       (($ <wasm> types imports funcs tables memories globals exports
           start elems datas tags strings custom)
        ;; A function can pull in types, funcs, tables, and globals from
        ;; the stdlib.  These fragments may be locally defined or
        ;; imported (except for types which are always locally defined).
        ;;
        ;; A table can pull in types and globals, possibly imported.
        ;;
        ;; A global can pull in types and globals, possibly imported.
        ;;
        ;; An elem can pull in types and globals, possibly imported.
        ;;
        ;; An import can pull in types.
        ;;
        ;; A tag can pull in types.
        ;;
        ;; A type can pull in other types.
        ;;
        ;; Therefore, we can allow pieces of the stdlib to lazily pull
        ;; in other pieces of the stdlib if we compute the different
        ;; pulled-in module components in this order: funcs, tables,
        ;; globals, imports, types.
        (let* ((funcs (compute-funcs funcs))
               (tables (compute-tables funcs tables))
               (globals (compute-globals funcs tables globals elems))
               (imports (compute-imports imports funcs tables globals elems))
               (types (compute-types types imports funcs tables globals elems
                                     tags)))
          (make-wasm types imports funcs tables memories globals exports
                     start elems datas tags strings custom)))))))

(define (lower-to-wasm cps)
  ;; interning constants into constant table
  ;; finalizing constant table
  ;; setting init function.
  (define strings '())
  (define heap-constants '())
  (define (func-label k) (string->symbol (format #f "$f~a" k)))
  (define funcs
    (intmap-map
     (lambda (kfun body)
       (let ((cps (intmap-select cps body)))
         (define preds (compute-predecessors cps kfun))
         (define idoms (compute-idoms cps kfun))
         (define dom-children (invert-tree idoms))
         (define (merge-cont? label)
           (let lp ((preds (intmap-ref preds label))
                    (has-forward-in-edge? #f))
             (match preds
               (() #f)
               ((pred . preds)
                (if (< pred label)
                    (or has-forward-in-edge?
                        (lp preds #t))
                    (lp preds has-forward-in-edge?))))))
         (define (loop-cont? label)
           (or-map (lambda (pred) (<= label pred))
                   (intmap-ref preds label)))
         (define (loop-label label)
           (string->symbol (format #f "$l~a" label)))
         (define (wrap-loop expr label)
           (if (loop-cont? label)
               `(loop ,(loop-label label) ,void-block-type ,expr)
               expr))
         (define (var-label var) (string->symbol (format #f "$v~a" var)))
         (define (local.get var) `(local.get ,(var-label var)))
         (define (local.set var) `(local.set ,(var-label var)))
         (define (local-arg-label idx) (string->symbol (format #f "$arg~a" idx)))
         (define (global-arg-label idx) (string->symbol (format #f "$arg~a" idx)))
         (define (arg-ref idx)
           (cond
            ((< idx 3) `(local.get ,(local-arg-label idx)))
            ((< idx 8) `(global.get ,(global-arg-label idx)))
            (else `(table.get $argv (i32.const ,(- idx 8))))))
         (define (compile-tail exp)
           (define (pass-abi-arguments args)
             (cons
              `(i32.const ,(length args))
              (let lp ((args args) (idx 0))
                (match args
                  (()
                   (if (< idx 3)
                       (append '((i32.const 0)
                                 (i31.new))
                             (lp args (1+ idx)))
                       '()))
                  ((arg . args)
                   (cons (cond
                          ((< idx 3) (local.get arg))
                          ((< idx 8)
                           `(global.set ,(global-arg-label idx)
                                        ,(local.get arg)))
                          (else
                           `(table.set $argv (i32.const ,(- idx 8))
                                       ,(local.get arg))))
                         (lp args (1+ idx))))))))
           (match exp
             (($ $call proc args)
              `(,@(pass-abi-arguments args)
                ,(local.get proc)
                (ref.cast $proc)
                (struct.get $proc 1)
                (return_call_ref ,(make-type-use '$kvarargs kvarargs-sig))))
             (($ $calli args callee)
              ;; This is a return.
              `(,@(pass-abi-arguments args)
                ,(local.get callee)
                (return_call_ref ,(make-type-use '$kvarargs kvarargs-sig))))
             (($ $callk k proc args)
              `(,@(map local.get (if proc (cons proc args) args))
                (return_call ,(func-label k))))))
         (define (compile-values exp)
           (define (fixnum? val)
             (and (exact-integer? val)
                  (<= (ash -1 -29) val (1- (ash 1 29)))))
           (match exp
             (($ $const val)
              (match val
                ((? fixnum?) `((i32.const ,(ash val 1))
                               (i31.new)))
                (_ (error "unimplemented constant" val))))
             (($ $primcall 'restore1 'ptr ())
              `((call $pop-return!)))
             (_
              (error "unimplemented!" exp))))
         (define (compile-receive exp req rest kargs)
           (match exp
             (_
              (error "unimplemented!!" exp req rest kargs))))
         (define (compile-test op param args)
           (match op
             (_ (error "unimplemented!!!" op param args))))
         
         ;; See "Beyond Relooper: Recursive Translation of Unstructured
         ;; Control Flow to Structured Control Flow", Norman Ramsey, ICFP
         ;; 2022.
         (define (make-ctx next-label stack) (cons next-label stack))
         (define (push-loop label ctx)
           (match ctx
             ((next-label . stack)
              (make-ctx label
                        (acons 'loop-headed-by label stack)))))
         (define (push-block label ctx)
           (match ctx
             ((next-label . stack)
              (make-ctx label
                        (acons 'block-followed-by label stack)))))
         (define (push-if label ctx)
           (match ctx
             ((next-label . stack)
              (make-ctx next-label (cons 'if-then-else stack)))))
         (define (lookup-label k ctx)
           (match ctx
             ((next-label . stack)
              (let lp ((stack stack) (depth 0))
                (match stack
                  (('if-then-else . stack) (lp stack (1+ depth)))
                  ((((or 'loop-headed-by 'block-followed-by) label) . stack)
                   (if (eqv? label k)
                       depth
                       (lp stack (1+ depth))))
                  (_ (error "block label not found" k)))))))

         (define (do-tree label ctx)
           (define (code-for-label ctx)
             ;; here if label is a switch we node-within all children
             ;; instead of only merge nodes.
             (define children
               (intset-filter merge-cont? (intmap-ref dom-children label)))
             (node-within label children ctx))
           (if (loop-cont? label)
               `((loop #f #f ,(code-for-label (push-loop label ctx))))
               (code-for-label ctx)))
         (define (do-branch pred succ ctx)
           (cond
            ((or (<= succ pred)
                 (merge-cont? succ))
             ;; Backward branch or branch to merge: jump.
             (match ctx
               ((next-label . stack)
                (if (eqv? succ next-label)
                    '()
                    `((br ,(lookup-label succ ctx)))))))
            (else
             ;; Otherwise render successor inline.
             (do-tree succ ctx))))
         (define (node-within label ys ctx)
           (call-with-values (lambda () (intset-pop ys))
             (lambda (ys y)
               (match y
                 (#f
                  (match (intmap-ref cps label)
                    (($ $kargs names vars term)
                     ;; could change to bind names at continuation?
                     (match term
                       (($ $continue k src exp)
                        (match (intmap-ref cps k)
                          (($ $ktail)
                           (compile-tail exp))
                          (($ $kargs _ vars)
                           `(,@(compile-values exp)
                             ,@(reverse (map local.set vars))
                             ,@(do-branch label k ctx)))
                          (($ $kreceive ($ $arity req () rest () #f) kargs)
                           (compile-receive exp req rest kargs))))
                       (($ $branch kf kt src op param args)
                        `(,@(compile-test op param args)
                          (if #f ,void-block-type
                              ,(do-branch label kt (cons 'if-then-else ctx))
                              ,(do-branch label kf (cons 'if-then-else ctx)))))
                       (($ $switch kf kt* src arg)
                        (error "switch unimplemented"))
                       (($ $prompt k kh src escape? tag)
                        (error "prompts should be removed by tailification?"))
                       (($ $throw src op param args)
                        (error "throw unimplemented"))))
                    (($ $kreceive ($ $arity req () rest () #f) kbody)
                     (error "kreceive unimplemented"))
                    (($ $kfun src meta self ktail kentry)
                     (if self
                         ;; only if referenced?
                         `(,(arg-ref 0)
                           ,(local.set self)
                           ,@(do-branch label kentry ctx))
                         (do-tree kentry ctx)))
                    (($ $kclause ($ $arity req opt rest kw allow-other-keys?)
                        kbody kalt)
                     (when kalt (error "case-lambda unimplemented"))
                     (when allow-other-keys? (error "allow-other-keys? unimplemented"))
                     (when (not (null? kw)) (error "kwargs unimplemented"))
                     (when (not (null? opt)) (error "optargs unimplemented"))
                     (when rest (error "rest args unimplemented"))
                     (match (intmap-ref cps kbody)
                       (($ $kargs names vars)
                        `((local.get $nargs)
                          (i32.const ,(1+ (length req)))
                          (i32.eq)
                          (if #f ,void-block-type
                              (,@(append-map (lambda (arg idx)
                                               `(,(arg-ref (1+ idx))
                                                 ,(local.set arg)))
                                             vars (iota (length req)))
                               ,@(do-branch label kbody ctx))
                              ((unreachable)))))))
                    (($ $ktail)
                     '())))
                 (y
                  `((block #f ,void-block-type
                           ,(node-within label ys (push-block label ctx)))
                    ,@(do-tree y ctx)))))))
         (define code (do-tree kfun (make-ctx #f '())))
         (define (type-for-repr repr)
           (match repr
             ('scm scm-type)
             ('f64 'f64)
             ((or 's64 'u64) 'i64)
             ('ptr (make-ref-type #f '$kvarargs))))
         (define locals
           (intmap-fold-right (lambda (var repr out)
                                (cons (make-local (var-label var)
                                                  (type-for-repr repr))
                                      out))
                              (compute-var-representations cps) '()))
         ;; FIXME: Here attach a name, other debug info to the function
         (make-func (func-label kfun)
                    (make-type-use '$kvarargs kvarargs-sig)
                    locals
                    code)))
     (compute-reachable-functions cps 0)))
  (define types '())
  (define imports '())
  (define tables '())
  (define memories '())
  (define globals '()) ;; FIXME: heap constants
  (define exports '())
  (define start #f) ;; FIXME: heap constants
  (define elems '())
  (define datas '())
  (define tags '())
  (define custom '())
  (make-wasm types imports
             (intmap-fold-right (lambda (kfun func funcs) (cons func funcs))
                                funcs '())
             tables memories globals exports
             start elems datas tags strings custom))

(define* (compile-to-wasm input-file output-file #:key
                          (import-abi? #f)
                          (from (current-language))
                          (env (default-environment from))
                          (optimization-level (default-optimization-level))
                          (warning-level (default-warning-level))
                          (opts '())
                          (canonicalization 'relative))
  (define (compile-to-cps in)
    ;; FIXME: Right now the tree-il->cps phase will expand
    ;; primitives to Guile VM primitives, e.g. including
    ;; `heap-object?` and so on.  We need to instead expand into
    ;; more wasm-appropriate primitives, at some point anyway.
    (define cps
      (read-and-compile in #:env env #:from from #:to 'cps
                        #:optimization-level optimization-level
                        #:warning-level warning-level))
    (define lower-cps
      (let ((make-lower (language-lowerer (lookup-language 'cps))))
        (make-lower optimization-level opts)))
    (define lowered-cps (lower-cps cps env))
    (define tailified (tailify lowered-cps))
    (verify tailified)
    (renumber (simplify (eliminate-dead-code tailified))))
  (call-with-input-file input-file
    (lambda (in)
      (set-port-encoding! in (or (file-encoding in) "UTF-8"))
      (define cps (compile-to-cps in))
      (dump cps)
      (let ((wasm (resolve-wasm (add-stdlib (lower-to-wasm cps) import-abi?))))
        (format #t "\n\nThe wasm we are going to emit:\n")
        (dump-wasm wasm)
        (let ((bytes (assemble-wasm wasm)))
          (call-with-output-file output-file
            (lambda (out)
              (put-bytevector out bytes))))))))

(define (main args)
  (match args
    ((_ in out)
     (compile-to-wasm in out)
     (format #t "\n\nParsing the wasm we emitted:\n")
     (dump-wasm (call-with-input-file out parse-wasm)))
    ((arg0 . _)
     (format (current-error-port) "usage: ~a INPUT-FILE OUTPUT-FILE\n" arg0)
     (exit 1))))

(when (batch-mode?)
  (main (program-arguments))
  (exit 0))
