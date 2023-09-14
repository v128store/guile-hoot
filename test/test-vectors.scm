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
;;; Vector tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-vectors")

(test-call "#(1 2 3)" (lambda (a b c) (vector a b c)) 1 2 3)
(test-call "3" (lambda (v) (vector-ref v 2)) #(1 2 3))
(test-call "2" (lambda (v idx) (vector-ref v idx)) #(1 2 3) 1)
(test-call "#(42 42 42)" (lambda (n) (make-vector n 42)) 3)

(test-end* "test-vectors")
