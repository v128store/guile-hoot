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
;;; String tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-strings")

(test-call "3" (lambda (str) (string-length str)) "fox")
(test-call "#\\f" (lambda (str) (string-ref str 0)) "fox")
(test-call "#\\x" (lambda (str) (string-ref str 2)) "fox")
(test-call "#t" (lambda (a b) (string=? a b)) "Even" "Even")
(test-call "#t" (lambda (a b) (string>? a b)) "Odd" "Even")
(test-call "#t" (lambda (a b) (string<? a b)) "Even" "Odd")

(test-call "\"abc\"" (lambda (a) (string-copy a)) "abc")

;; String mutation
(test-call "#f" (lambda (a) (mutable-string? a)) "abc")
(test-call "#t" (lambda () (mutable-string? (make-string 1))))
(test-call "#t" (lambda () (mutable-string? (string-copy "abc"))))
(test-call "#t" (lambda () (mutable-string? (string #\a #\b #\c))))
(test-call "\"1@3\"" (lambda (a)
                       (let ((a (string-copy a)))
                         (string-set! a 1 #\@)
                         a))
           "123")
(test-call "\"a123e\"" (lambda (a)
                         (let ((b (string-copy "abcde")))
                           (string-copy! b 1 a 0 3)
                           b))
           "12345")
(test-call "\"a!!!!f\"" (lambda ()
                          (let ((a (string-copy "abcdef")))
                            (string-fill! a #\! 1 5)
                            a)))

(test-end* "test-strings")
