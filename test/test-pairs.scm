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
;;; Pair tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-pairs")

(test-call "(1 . 2)" (lambda (a b) (cons a b)) 1 2)
(test-call "1" (lambda (a) (car a)) '(1 . 2))
(test-call "2" (lambda (a) (cdr a)) '(1 . 2))

(test-call "#t" (lambda (a b) (equal? a b)) '() '())
(test-call "#t" (lambda (a b) (equal? a b)) '(1 . 2) '(1 . 2))
(test-call "#t" (lambda (a b) (equal? a b)) '(1 2) '(1 2))
(test-call "#f" (lambda (a b) (equal? a b)) '() '(1))
(test-call "#f" (lambda (a b) (equal? a b)) '(1 2) '(2 1))

(test-end* "test-pairs")
