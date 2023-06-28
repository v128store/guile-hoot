;;; WebAssembly assembler
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
;;; Assembler for WebAssembly.
;;;
;;; Code:

(define-module (wasm types)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:export (find-type))  ; the rest are implicitly exported

(define-syntax define-simple-record-type
  (lambda (x)
    (define (id-append id . parts)
      (datum->syntax id (apply symbol-append parts)))
    (syntax-case x ()
      ((_ <rt> field ...)
       (let ((stem (let ((str (symbol->string (syntax->datum #'<rt>))))
                     (string->symbol
                      (substring str 1 (1- (string-length str)))))))
         (with-syntax ((make-rt (id-append #'<rt> 'make- stem))
                       (pred (id-append #'<rt> stem '?))
                       ((getter ...) (map (lambda (f)
                                            (id-append #'<rt> stem '-
                                                       (syntax->datum f)))
                                          #'(field ...))))
           #'(begin
               (define-record-type <rt>
                 (make-rt field ...)
                 pred
                 (field getter)
                 ...)
               (export <rt>)
               (export make-rt)
               (export pred)
               (export getter) ...)))))))
(define-syntax-rule (define-simple-record-types (rt field ...) ...)
  (begin
    (define-simple-record-type rt field ...)
    ...))
(define-simple-record-types
  (<wasm> types imports funcs tables memories globals exports
          start elems datas tags strings custom)
  (<param> id type)
  (<func-sig> params results)
  (<type-use> idx sig)
  (<ref-type> nullable? heap-type)
  (<limits> min max)
  (<table-type> limits elem-type)
  (<mem-type> limits)
  (<global-type> mutable? type)
  (<array-type> mutable? type)
  (<field> id mutable? type)
  (<struct-type> fields)
  (<sub-type> final? supers type)
  (<rec-group> types)
  (<type> id val)
  (<import> mod name kind id type)
  (<export> name kind idx)
  (<mem-arg> id offset align)
  (<elem> id mode table type offset inits)
  (<data> id mode mem offset init)
  (<tag> id type)
  (<local> id type)
  (<func> id type locals body)
  (<table> id type init)
  (<memory> id type)
  (<global> id type init)
  (<custom> name bytes))

(define (find-type pred types)
  (let lp ((types types) (idx 0))
    (define (visit-base rec supers type-id type-idx type)
      (pred rec type-id type-idx supers type))
    (define (visit-sub rec type-id type-idx type)
      (match type
        (($ <sub-type> final? supers type)
         (visit-base rec supers type-id type-idx type))
        (_ (visit-base rec '()  type-id type-idx type))))
    (match types
      (() #f)
      ((($ <rec-group> subtypes) . types)
       (let ((rec idx))
         (let lp2 ((subtypes subtypes) (idx idx))
           (match subtypes
             (() (lp types idx))
             ((($ <type> id subtype) . subtypes)
              (or (visit-sub rec id idx subtype)
                  (lp2 subtypes (1+ idx))))))))
      ((($ <type> id type) . types)
       (or (visit-sub idx id idx type)
           (lp types (1+ idx)))))))
