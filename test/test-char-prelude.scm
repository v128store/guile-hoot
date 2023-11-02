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

(define-syntax-rule (define-char-prelude-procedures (name name*) ...)
  (define-values (name* ...)
    (let ()
      (include-from-path "hoot/char-prelude.scm")
      (values name ...))))

(define-char-prelude-procedures
  (char-upcase char-upcase*)
  (char-downcase char-downcase*)
  (char-upper-case? char-upper-case?*)
  (char-lower-case? char-lower-case?*))

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

(test-assert "char-upcase"
             (unary-char-procs-same? char-upcase char-upcase*))
(test-assert "char-downcase"
             (unary-char-procs-same? char-downcase char-downcase*))
(test-assert "char-upper-case?"
             (unary-char-procs-same? char-upper-case? char-upper-case?*))
(test-assert "char-lower-case?"
             (unary-char-procs-same? char-lower-case? char-lower-case?*))

(test-end* "test-char-prelude")
