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

(define-module (wasm dump)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module ((srfi srfi-1) #:select (count))
  #:use-module (wasm types)
  #:export (dump-wasm))

(define* (dump-wasm mod #:key (port (current-output-port)))
  (define (enumerate f items start)
    (let lp ((items items) (idx start))
      (match items
        (() (values))
        ((item . items)
         (f item idx)
         (lp items (1+ idx))))))

  (define* (dump-items header items #:optional (start 0))
    (unless (null? items)
      (format port "~a:\n" header)
      (enumerate (lambda (item idx)
                   (format port "  ~a: ~a\n" idx item))
                 items start)
      (newline port)))

  (define (val-type-repr vt)
    (match vt
      (($ <ref-type> #t ht)
       `(ref null ,ht))
      (($ <ref-type> #f ht)
       `(ref ,ht))
      (_ vt)))

  (define (type-repr type)
    (define (params-repr params)
      (match params
        ((($ <param> #f type) ...)
         `((param ,@(map val-type-repr type))))
        (((($ <param> id type) . params))
         (cons `(param ,id ,(val-type-repr type))
               (params-repr params)))))
    (define (results-repr results)
      `(result ,@(map val-type-repr results)))
    (define (field-repr field)
      (define (wrap mutable? repr)
        (if mutable? `(mut ,repr) repr))
      (match field
        (($ <field> id mutable? type)
         (let ((repr (wrap mutable? (val-type-repr type))))
           (if id
               `(field ,id ,repr)
               repr)))))
    (match type
      (($ <func-sig> params results)
       `(func ,@(params-repr params) ,@(results-repr results)))
      (($ <sub-type> supers type)
       `(sub ,@supers ,(type-repr type)))
      (($ <struct-type> fields)
       `(struct ,@(map field-repr fields)))
      (($ <array-type> mutable? type)
       `(array ,(field-repr (make-field #f mutable? type))))))

  (define (type-use-repr type)
    (match type
      (($ <type-use> idx ($ <type> id type))
       (type-repr type))
      (($ <type-use> idx type)
       `(type ,(or idx "error: invalid type use!")))))

  (define (dump-types types)
    (define (dump-type type idx indent)
      (match type
        (($ <type> id type)
         (let ((repr (type-repr type)))
           (format port "~a~a~@[ (~a)~]: ~a\n" indent idx id repr)))))
    (unless (null? types)
      (format port "Types:\n")
      (let lp ((types types) (idx 0))
        (match types
          (() (values))
          ((($ <rec-group> rec-types) . types)
           (format port "Recursive type group:\n")
           (enumerate (lambda (type idx)
                        (dump-type type idx "    "))
                      rec-types idx)
           (format port "Recursive type group end.\n")
           (lp types (+ idx (length rec-types))))
          ((type . types)
           (dump-type type idx "  ")
           (lp types (1+ idx)))))
      (newline port)))

  (define (dump-imports imports)
    (dump-items "Imports" imports))

  (define (dump-func-decls funcs imported)
    (dump-items "Function declarations"
                (map (match-lambda (($ <func> id type locals body)
                                    (type-use-repr type)))
                     funcs)
                imported))

  (define (dump-tables tables imported)
    (dump-items "Tables" tables imported))

  (define (dump-memories memories imported)
    (dump-items "Memories" memories))

  (define (dump-tags tags)
    (dump-items "Tags" tags))

  (define (dump-strings strings)
    (dump-items "Strings" strings))

  (define (dump-globals globals imported)
    (dump-items "Globals" globals imported))

  (define (dump-exports exports)
    (dump-items "Exports" exports))

  (define (dump-start start)
    (when start
      (format port "Start: #~a\n\n" start)))

  (define (dump-elems elems)
    (dump-items "Elems" elems))

  (define (dump-data datas)
    (dump-items "Datas" datas))

  (define (dump-func-defs funcs imported)
    (unless (null? funcs)
      (format port "Function definitions:\n")
      (enumerate
       (match-lambda*
        ((($ <func> id type locals body) idx)
         (format port "  Function #~a:\n" idx)
         (when id (format port "    Id: ~a\n" id))
         (format port "    Type: ~a\n" (type-use-repr type))
         (match locals
           (() #t)
           ((($ <local> id vt) ...)
            (format port "    Locals:~:{ ~@[~a:~]~a~}\n"
                    (map list id (map val-type-repr vt)))))
         (format port "    Body:\n")
         (pretty-print body #:port port #:per-line-prefix "      ")))
       funcs
       imported)))

  (match mod
    (($ <wasm> types imports funcs tables memories globals exports start
        elems datas tags strings custom)
     (define (import-has-kind kind)
       (match-lambda
        (($ <import> mod name kind' id type) (eq? kind kind'))))
     (let ((imported-funcs (count (import-has-kind 'func) imports))
           (imported-tables (count (import-has-kind 'table) imports))
           (imported-memories (count (import-has-kind 'memory) imports))
           (imported-globals (count (import-has-kind 'global) imports)))
       (dump-types types)
       (dump-imports imports)
       (dump-func-decls funcs imported-funcs)
       (dump-tables tables imported-tables)
       (dump-memories memories imported-memories)
       (dump-tags tags)
       (dump-strings strings)
       (dump-globals globals imported-globals)
       (dump-exports exports)
       (dump-start start)
       (dump-elems elems)
       (dump-data datas)
       (dump-func-defs funcs imported-funcs)))))