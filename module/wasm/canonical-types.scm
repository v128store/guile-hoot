;;; WebAssembly VM
;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
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
;;; Reference type canonicalization.
;;;
;;; Code:

(define-module (wasm canonical-types)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (wasm types)
  #:export (canonicalize-types!
            canonicalize-type!))

;; A bit of global state for ref type canonicalization across modules.
(define *canonical-groups* (make-hash-table))

(define (canonicalize-types! types)
  ;; Create a vector big enough to hold all of the resulting types.
  (let ((canonical-vec (make-vector
                        (fold (lambda (type sum)
                                (match type
                                  (($ <rec-group> types)
                                   (+ sum (length types)))
                                  (_ (+ sum 1))))
                              0 types))))
    (define (visit-group types group-start)
      ;; Rolling up a type replaces indices outside of the type group
      ;; with a canonical type descriptor and indices inside of the
      ;; type group with relative indices.  This creates a new type
      ;; that can be equal? tested against a type from another module.
      (define (roll-up type group-start)
        (match type
          ((? symbol?) type)
          ((? exact-integer? idx)
           (if (< type group-start)
               `(outer ,(vector-ref canonical-vec idx))
               (- idx group-start)))
          (($ <ref-type> nullable? heap-type)
           (make-ref-type nullable? (roll-up heap-type group-start)))
          (($ <func-sig> params results)
           (make-func-sig (map (match-lambda
                                 (($ <param> _ type)
                                  (make-param #f (roll-up type group-start))))
                               params)
                          (map (lambda (type)
                                 (roll-up type group-start))
                               results)))
          (($ <struct-type> fields)
           (make-struct-type
            (map (match-lambda
                   (($ <field> _ mutable? type)
                    (make-field #f mutable? (roll-up type group-start))))
                 fields)))
          (($ <array-type> mutable? type)
           (make-array-type mutable? (roll-up type group-start)))
          (($ <sub-type> final? supers type)
           (make-sub-type final?
                          (map (lambda (super)
                                 (roll-up super group-start))
                               supers)
                          (roll-up type group-start)))))
      ;; If a type group with identical structure has already been
      ;; canonicalized, return the cached type descriptors.  Otherwise,
      ;; generate new ones, cache them, and return them.
      (let ((types* (map (match-lambda
                           (($ <type> id type)
                            (roll-up type group-start)))
                         types)))
        (match (hash-ref *canonical-groups* types*)
          ;; Cache hit: Just copy 'em over.
          ((? vector? cached-group)
           (do ((i 0 (+ i 1)))
               ((= i (vector-length cached-group)))
             (vector-set! canonical-vec (+ group-start i)
                          (vector-ref cached-group i)))
           (+ group-start (vector-length cached-group)))
          ;; Cache miss: Generate and cache new descriptors.
          (#f
           (let ((group-vec (make-vector (length types))))
             (let loop ((types types*)
                        (i 0))
               ;; Unrolling a type replaces relative recursive type
               ;; indices with canonical type references.
               (define (unroll type)
                 (match type
                   ((? symbol?) type)
                   ;; Types may have recursive references to other
                   ;; types within the same group, so we're lazy about
                   ;; it.
                   ((? exact-integer? idx)
                    (delay (vector-ref group-vec idx)))
                   ;; Types from outside the group are already
                   ;; unrolled so recursion stops.
                   (('outer type) type)
                   (($ <ref-type> nullable? heap-type)
                    (make-ref-type nullable? (unroll heap-type)))
                   (($ <func-sig> params results)
                    (make-func-sig (map (match-lambda
                                          (($ <param> _ type)
                                           (make-param #f (unroll type))))
                                        params)
                                   (map unroll results)))
                   (($ <struct-type> fields)
                    (make-struct-type
                     (map (match-lambda
                            (($ <field> _ mutable? type)
                             (make-field #f mutable? (unroll type))))
                          fields)))
                   (($ <array-type> mutable? type)
                    (make-array-type mutable? (unroll type)))
                   (($ <sub-type> final? supers type)
                    (make-sub-type final?
                                   (map unroll supers)
                                   (unroll type)))))
               (match types
                 (()
                  (hash-set! *canonical-groups* types* group-vec)
                  (+ group-start i))
                 ((type . rest)
                  (let ((type* (unroll type)))
                    (vector-set! group-vec i type*)
                    (vector-set! canonical-vec (+ group-start i) type*)
                    (loop rest (+ i 1)))))))))))
    ;; Visit all the type groups and canonicalize them.  A type that
    ;; is not in a recursive type group is treated as being in a group
    ;; of one.
    (let loop ((groups types)
               (i 0))
      (match groups
        (() #t)
        ((($ <rec-group> (types ...)) . rest)
         (loop rest (visit-group types i)))
        (((? type? type) . rest)
         (loop rest (visit-group (list type) i)))))
    ;; Generate a new type list using the canonical types.
    (let loop ((types (append-map (match-lambda
                                    (($ <rec-group> types) types)
                                    ((? type? type) (list type)))
                                  types))
               (i 0))
      (match types
        (() '())
        ((($ <type> id type) . rest)
         (cons (make-type id (vector-ref canonical-vec i))
               (loop rest (+ i 1))))))))

;; Convenience procedure for canonicalizing individual types outside
;; of a module context.  Useful in the stack effect and reflection
;; modules, for example.
(define (canonicalize-type! type)
  (match (canonicalize-types! (list (make-type #f type)))
    ((($ <type> _ val)) val)))
