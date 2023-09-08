;;; WebAssembly inline assembly
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
;;; Compiler support for inline assembly.
;;;
;;; Code:

(define-module (hoot inline-wasm)
  #:use-module (ice-9 match)
  #:use-module ((language tree-il primitives)
                #:select (add-interesting-primitive!))
  #:use-module ((language tree-il compile-cps)
                #:select (define-custom-primcall-converter))
  #:use-module (language tree-il)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (wasm types)
  #:use-module (wasm wat)
  #:use-module ((hoot primitives) #:select (%inline-wasm))
  #:export (install-inline-wasm!))

(define install-inline-wasm!
  (let ((m (current-module)))
    (lambda ()
      ;; The useless reference is to prevent a warning that (language
      ;; tree-il primitives) is unused; we just import the module so that we
      ;; can add %inline-asm as a primitive, because for reasons I don't
      ;; understand, you can't call add-interesting-primitive! from within
      ;; the compilation unit that defines the primitive.
      %inline-wasm
      (save-module-excursion
       (lambda ()
         (set-current-module m)
         (add-interesting-primitive! '%inline-wasm))))))

(define (n-valued-continuation cps src nvals k)
  (define (enumerate f n)
    (let lp ((i 0))
      (if (< i n)
          (cons (f i) (lp (1+ i)))
          '())))
  (match (intmap-ref cps k)
    (($ $ktail)
     (let ((names (enumerate (lambda (n) 'result) nvals))
           (temps (enumerate (lambda (n) (fresh-var)) nvals)))
       (with-cps cps
         (letk k* ($kargs names temps
                    ($continue k src ($values temps))))
         k*)))
    (($ $kreceive ($ $arity req () rest () #f) kargs)
     (cond
      ((and (not rest) (= nvals (length req)))
       (with-cps cps
         kargs))
      ((and rest (= nvals (length req)))
       (let ((names (enumerate (lambda (n) 'result) nvals))
             (temps (enumerate (lambda (n) (fresh-var)) nvals)))
         (with-cps cps
           (letv rest)
           (letk knil ($kargs ('rest) (rest)
                         ($continue kargs src
                           ($values ,(append temps (list rest))))))
           (letk k ($kargs names temps
                     ($continue knil src ($const '()))))
           k)))
      ((and rest (zero? (length req)))
       ;; Very annoyingly, this can happen as a result of the
       ;; compilation of e.g. (letrec ((x A)) B), where X is not used in
       ;; B.  This gets compiled to (<seq> A B), and when the CPS
       ;; converter doesn't know that A is zero-valued, it just makes a
       ;; (lambda ignored B) continuation.  This happens to us when
       ;; prelude bindings that are inline-wasm forms are unused in a
       ;; user program.  So, we cons it up!
       (let ((names (enumerate (lambda (n) 'result) nvals))
             (temps (enumerate (lambda (n) (fresh-var)) nvals)))
         (define (cons-values cps temps k)
           (match temps
             (()
              (with-cps cps
                (build-term
                  ($continue k src ($const '())))))
             ((temp . temps)
              (with-cps cps
                (letv rest)
                (letk kcons ($kargs ('rest) (rest)
                              ($continue k src
                                ($primcall 'cons #f (temp rest)))))
                ($ (cons-values temps kcons))))))
         (with-cps cps
           (let$ term (cons-values temps kargs))
           (letk k ($kargs names temps ,term))
           k)))
      (else
       (error "unexpected continuation for n-valued result" nvals))))))

(define-custom-primcall-converter (%inline-wasm cps src args convert-args k)
  (define-syntax-rule (assert-match x pat message)
    (match x
      (pat #t)
      (_ (error message x))))
  (match args
    ((($ <const> _ code) . args)
     (assert-match code ('func . _)
                   "inline-wasm: expected a single (func ...)")
     (match (parse-wat (list code))
       ;; We expect a single func and no other definitions (types,
       ;; tables, etc).
       (($ <wasm> () ()
           ((and func ($ <func> id ($ <type-use> #f
                                      ($ <func-sig> params results))
                         locals body)))
           () () () () #f () () () () ())
        (assert-match params (($ <param> id ($ <ref-type> #f 'eq)) ...)
                      "inline asm requires all params to be (ref eq)")
        (assert-match results (($ <ref-type> #f 'eq) ...)
                      "inline asm requires all results to be (ref eq)")
        (unless (= (length params) (length args))
          (error "inline asm with incorrect number of args" code))
        (convert-args cps args
          (lambda (cps args)
            (with-cps cps
              (let$ k* (n-valued-continuation src (length results) k))
              (build-term
                ($continue k* src
                  ($primcall 'inline-wasm func args)))))))))))
