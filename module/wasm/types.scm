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

(define-module (wasm types)
  #:export (<wasm> make-wasm
            <param> make-param
            <func-sig> make-func-sig
            <type-use> make-type-use
            <ref-type> make-ref-type
            <array-type> make-array-type
            <field> make-field
            <struct-type> make-struct-type
            <sub-type> make-sub-type
            <rec-group> make-rec-group
            <limits> make-limits
            <table-type> make-table-type
            <mem-type> make-mem-type
            <global-type> make-global-type
            <type> make-type
            <import> make-import
            <export> make-export
            <mem-arg> make-mem-arg
            <elem> make-elem
            <data> make-data
            <tag> make-tag
            <local> make-local
            <func> make-func
            <table> make-table
            <memory> make-memory
            <global> make-global
            <custom> make-custom))

(define-syntax define-simple-record-type
  (lambda (x)
    (define (id-append id . parts)
      (datum->syntax id (apply symbol-append parts)))
    (syntax-case x ()
      ((_ <rt> field ...)
       (let ((stem (let ((str (symbol->string (syntax->datum #'<rt>))))
                     (string->symbol
                      (substring str 1 (1- (string-length str)))))))
         (with-syntax ((make-rt (id-append #'<rt> 'make- stem)))
           #'(begin
               (define <rt> (make-record-type '<rt> '(field ...)))
               (define make-rt (record-constructor <rt>)))))))))
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
  (<sub-type> supers type)
  (<rec-group> types)
  (<type> id val)
  (<import> mod name kind id type)
  (<export> name kind idx)
  (<mem-arg> offset align)
  (<elem> id mode table type offset inits)
  (<data> id mode mem offset init)
  (<tag> id type)
  (<local> id type)
  (<func> id type locals body)
  (<table> id type init)
  (<memory> id type)
  (<tag> id type)
  (<global> id type init)
  (<custom> name bytes))
