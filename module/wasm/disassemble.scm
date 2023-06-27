;;; WebAssembly assembler
;;; Copyright (C) 2023 Robin Templeton
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
;;; Disassembler for WebAssembly.
;;;
;;; Code:

(define-module (wasm disassemble)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (wasm types)
  #:export (disassemble))

(define (disassemble mod)
  (match mod
    (($ <wasm> types imports funcs tables memories globals exports start
               elems datas tags strings custom)
     `(wasm
       (types
        ,@(map (lambda (type)
                 (match type
                   (($ <type> id ($ <func-sig> params results))
                    `(type ,id (func ,@(map (lambda (param)
                                              (match param
                                                (($ <param> id type)
                                                 `(param ,type))))
                                            params)
                                     (result ,@results))))))
               types))
       (imports
        ,@(map (lambda (import)
                 (match import
                   (($ <import> mod name kind id type)
                    `(import ,mod ,name)
                    (case kind
                      ((memory)
                       (match type
                         (($ <mem-type> ($ <limits> min max))
                          `(import ,mod
                                   ,name
                                   ,(if max
                                        `(memory ,min ,max)
                                        `(memory ,min))))))
                      ((func)
                       (match type
                         ((or ($ <type-use> idx ($ <func-sig> params results))
                              ($ <type-use> idx
                                            ($ <type> _
                                                      ($ <func-sig> params
                                                                    results))))
                          `(import ,mod
                                   ,name
                                   ,@(map (lambda (param)
                                            (match param
                                              (($ <param> id type)
                                               `(param ,id ,type)))
)
                                          params)
                                   (result ,@results)))))))))
               imports))       
       (funcs
        ,@(map (lambda (func)
                 (match func
                   (($ <func> id type locals body)
                    (match type
                      (($ <type-use> idx
                                     (or ($ <func-sig> params results)
                                         ($ <type> _ ($ <func-sig> params results))))
                       `(func ,(or id idx)
                              ,@(map (lambda (param)
                                       (match param
                                         (($ <param> id type)
                                          `(param ,id ,type))))
                                     params)
                              (result ,@results)
                              ,@(map (lambda (local)
                                       (match local
                                         (($ <local> id type)
                                          `(local ,id ,type))))
                                     locals)
                              ,@body))))))
               funcs))
       (tables ,@tables)           ;FIXME
       (memories ,@memories)       ;FIXME
       (globals
        ,@(map (lambda (global)
                 (match global
                   (($ <global> id ($ <global-type> mutable? type) init)
                    `(global ,id
                             ,(if mutable? `(mut ,type) type)
                             ,@init))))
               globals))
       (exports
        ,@(map (lambda (export)
                 (match export
                   (($ <export> name kind idx)
                    `(export ,name (,kind ,idx)))))
               exports))
       ,@(if start `((start ,start)) '())
       (elems
        ,@(map (lambda (elem)
                 (match elem
                   (($ <elem> id mode table type offset inits)
                    (case mode
                      ((passive)
                       `(elem ,id ,type ,@inits))
                      ((active)
                       `(elem ,id
                              (table ,table)
                              (offset ,offset)
                              ,type
                              ,@inits))
                      ((declarative)
                       `(elem ,id declare ,type ,@inits))))))
               elems))
       (datas
        ,@(map (lambda (data)
                 (match data
                   (($ <data> id mode mem offset init)
                    (case mode
                      ((active)
                       `(data ,id (memory ,mem) (offset ,@offset) ,init))
                      ((passive)
                       `(data ,id ,init))))))
               datas))
       (tags ,@tags)        ;FIXME
       (strings ,@strings) ;FIXME
       (custom ,custom)  ;FIXME
       ))))
