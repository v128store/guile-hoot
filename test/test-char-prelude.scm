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
;;; Tests for generated char-upcase, char-downcase, and so on.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (ice-9 format)
             (test utils))

(define (unary-char-procs-same? reference proc)
  (define success #t)
  (char-set-for-each
   (lambda (ch)
     (unless (eqv? (reference ch) (proc ch))
       (format (current-error-port) "mismatch for ~a on ~s: ~s vs ~s\n"
               reference ch (reference ch) (proc ch))
       (set! success #f)))
   char-set:full)
  success)

(test-begin "test-char-prelude")

(define-syntax-rule (define-char-prelude-procedures (name name*) ...)
  (define-values (name* ...)
    (let ()
      (include-from-path "hoot/char-prelude.scm")
      (values name ...))))

(define-syntax-rule (test-char-prelude-procedures (name name*) ...)
  (begin
    (define-char-prelude-procedures (name name*) ...)
    (test-assert 'name (unary-char-procs-same? name name*))
    ...))

(test-char-prelude-procedures
  (char-upcase char-upcase*)
  (char-downcase char-downcase*)
  (char-upper-case? char-upper-case?*)
  (char-lower-case? char-lower-case?*)
  (char-alphabetic? char-alphabetic?*)
  (char-numeric? char-numeric?*)
  (char-whitespace? char-whitespace?*))

(test-end* "test-char-prelude")
