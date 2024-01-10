;;; Pass to lower globals with non-constant initexprs
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
;;; Some global constants aren't.  A symbol literal, for example, needs
;;; to be interned in a symtab, and that's not a constant instruction
;;; from WebAssembly's point of view.  Interning a symbol needs to be
;;; done in a "start" function; only then will the WebAssembly global
;;; have its final value and be available for use.  For that reason
;;; other globals that are constant by themselves might not be, if they
;;; directly or indirectly reference a non-constant value.
;;;
;;; Our approach is to just let the compiler and stdlib use non-constant
;;; initialization expressions for globals.  We have the same
;;; precondition as in standard WebAssembly that the globals are sorted:
;;; initexprs can only reference globals with a lower index.  Then we
;;; run a post-pass to transform non-constant initexpr into
;;; placeholders, and synthesize a start function containing the needed
;;; initializations.
;;;
;;; Code:

(define-module (wasm lower-globals)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map fold))
  #:use-module (wasm types)
  #:export (lower-globals))

(define (lower-globals wasm)
  (match wasm
    (($ <wasm> id types imports funcs tables memories globals exports start
        elems datas tags strings custom)

     (define imported-func-count
       (fold (lambda (import count)
               (match import
                 (($ <import> mod name 'func) (1+ count))
                 (_ count)))
             0 imports))
     (define imported-global-count
       (fold (lambda (import count)
               (match import
                 (($ <import> mod name 'func) (1+ count))
                 (_ count)))
             0 imports))

     (define all-globals-constant? #t)
     (define deferred-initializations (make-hash-table))
     (define (constant-global? id-or-idx)
       (not (hashq-ref deferred-initializations id-or-idx)))

     (define (constant-expr? expr)
       (define (constant-inst? inst)
         (match inst
           (((or 'i32.const 'f32.const 'i64.const 'f64.const) _) #t)
           (('ref.null _) #t)
           (('ref.func _) #t)
           (('global.get g) (constant-global? g))
           (('struct.new _) #t)
           (('struct.new_default _) #t)
           (('array.new _) #t)
           (('array.new_default _) #t)
           (('array.new_fixed _ _) #t)
           (('extern.externalize) #t)
           (('extern.internalize) #t)
           (('ref.i31) #t)
           (('string.const _) #t)
           (_ #f)))
       (and-map constant-inst? expr))

     (define (mutable-nullable-type type)
       (match type
         (($ <global-type> mutable? type)
          (make-global-type
           #t
           (match type
             (($ <ref-type> nullable? ht) (make-ref-type #t ht))
             (_ type))))))

     (define (default-initializer type)
       (match type
         (($ <global-type> mutable? type)
          (match type
            (($ <ref-type> _ ht) `((ref.null ,ht)))
            ('i32 '((i32.const 0)))
            ('i64 '((i64.const 0)))
            ('f32 '((f32.const 0)))
            ('f64 '((f64.const 0)))))))

     (define lowered-globals
       (map (lambda (global idx)
              (match global
                (($ <global> id type (? constant-expr?))
                 global)
                (($ <global> id type init)
                 (set! all-globals-constant? #f)
                 (hashq-set! deferred-initializations idx global)
                 (when id
                   (hashq-set! deferred-initializations id global))
                 (make-global id (mutable-nullable-type type)
                              (default-initializer type)))))
            globals
            (iota (length globals) imported-global-count)))

     (define deferred-initexpr
       (append-map (lambda (idx)
                     (match (hashq-ref deferred-initializations idx)
                       (($ <global> id type init)
                        (append init `((global.set ,(or id idx)))))
                       (#f '())))
                   (iota (length globals) imported-global-count)))

     (define existing-start-func
       (and start
            (let lp ((funcs funcs) (idx imported-func-count))
              (match funcs
                (() (error "start func not found; is it imported?"))
                (((and func ($ <func> id type locals body)) . funcs)
                 (if (or (eq? idx start) (eq? id start))
                     func
                     (lp funcs (1+ idx))))))))

     (define existing-start-func-type
       (match existing-start-func
         (($ <func> id type locals body) type)
         (#f (or-map (match-lambda
                      (($ <type> id ($ <func-sig> () ()))
                       id)
                      (_ #f))
                     types))))
     (define new-start-func-type
       (or existing-start-func-type
           (make-type '$__start (make-func-sig '() '()))))
     (define new-types
       (if existing-start-func-type
           types
           (append types (list new-start-func-type))))

     (define new-start-func-id (or start '$__start))
     (define new-start-func
       (match existing-start-func
         (($ <func> id type locals body)
          (make-func id type locals (append deferred-initexpr body)))
         (#f
          (make-func new-start-func-id
                     (make-type-use (or existing-start-func-type '$__start)
                                    (make-func-sig '() '()))
                     '() deferred-initexpr))))

     (define new-funcs
       (let lp ((funcs funcs))
         (match funcs
           ((func . funcs)
            (if (eq? existing-start-func func)
                (cons new-start-func funcs)
                (cons func (lp funcs))))
           (() (list new-start-func)))))

     (define (rewrite-global-get-in-expr expr)
       (define (global-needs-cast? g)
         (match (hashq-ref deferred-initializations g)
           (($ <global> id
               ($ <global-type> mutable? ($ <ref-type> nullable? ht))
               initexpr)
            (not nullable?))
           (_ #f)))
       (match expr
         (() '())
         ((('global.get (? global-needs-cast? g)) . expr)
          `((global.get ,g) (ref.as_non_null)
            . ,(rewrite-global-get-in-expr expr)))
         ((inst . expr)
          (cons (match inst
                  (('block label type insts)
                   `(block ,label ,type ,(rewrite-global-get-in-expr insts)))
                  (('loop label type insts)
                   `(loop ,label ,type ,(rewrite-global-get-in-expr insts)))
                  (('if label type consequent alternate)
                   `(if ,label ,type ,(rewrite-global-get-in-expr consequent)
                        ,(rewrite-global-get-in-expr alternate)))
                  (('try label type body catches catch-all)
                   `(try ,label ,type
                         ,(rewrite-global-get-in-expr body)
                         ,(map rewrite-global-get-in-expr catches)
                         ,(and=> catch-all rewrite-global-get-in-expr)))
                  (('try_delegate label type body handler)
                   `(try_delegate ,label ,type
                                  ,(rewrite-global-get-in-expr body)
                                  ,handler))
                  (_ inst))
                (rewrite-global-get-in-expr expr)))))

     (define (rewrite-global-get-in-func func)
       (match func
         (($ <func> id type locals body)
          (make-func id type locals (rewrite-global-get-in-expr body)))))

     (define (rewrite-global-get-in-global global)
       (match global
         (($ <global> id type init)
          (make-global id type (rewrite-global-get-in-expr init)))))

     (define (rewrite-global-get-in-elem elem)
       (match elem
         (($ <elem> id mode table type offset inits)
          (make-elem id mode table type
                     (and=> offset rewrite-global-get-in-expr)
                     (map rewrite-global-get-in-expr inits)))))

     (if all-globals-constant?
         wasm
         (let ((funcs (map rewrite-global-get-in-func new-funcs))
               (globals (map rewrite-global-get-in-global lowered-globals))
               (elems (map rewrite-global-get-in-elem elems)))
           (make-wasm id
                      new-types
                      imports
                      funcs
                      tables
                      memories
                      globals
                      exports
                      new-start-func-id
                      elems
                      datas
                      tags
                      strings
                      custom))))))
