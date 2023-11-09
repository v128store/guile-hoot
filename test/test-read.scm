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
;;; Exception tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-read")

(define-syntax test-read
  (lambda (stx)
    (syntax-case stx ()
      ((_ input)
       (let ((repr
              (call-with-output-string
               (lambda (p)
                 (write (call-with-input-string
                         (string-append "#!r6rs " (syntax->datum #'input))
                         read)
                        p)))))
         #`(test-call #,repr
                      (lambda (str) (read (open-input-string str)))
                      input))))))

(define-syntax test-read-datum
  (lambda (stx)
    (syntax-case stx ()
      ((_ expr)
       (let ((repr (call-with-output-string
                    (lambda (p) (write (syntax->datum #'expr) p)))))
         #`(test-read #,repr))))))

(test-read-datum 1)
(test-read-datum 12)
(test-read-datum (1 2 3))
(test-read-datum "foo")
(test-read-datum "foo\nbar")
(test-read-datum #(1 2 3))
(test-read-datum #vu8(1 2 3))
(test-read-datum #*11001)
(test-read-datum #t)
(test-read-datum #f)
(test-read-datum #:foo)
(test-read-datum #nil)

(test-read "; foo\n1")
(test-read "(#!r6rs 10)")
(test-read "(#!fold-case HEY)")
(test-read "(#!no-fold-case HEY)")
(test-read "(x y . z)")
(test-read "[x y . z]")
(test-read "#xff")
(test-read "10.5")
(test-read "#;42 69")
(test-read "#;42 69")
(test-read "\"\\x61;\"")
(test-read "#true")
(test-read "#false")

(test-end* "test-read")
