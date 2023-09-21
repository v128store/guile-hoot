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
;;; Bitwise operation tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-bitwise")

;; FIXME: add tests with bignum arguments
(test-call "8" (lambda (a b) (logand a b)) #b1100 #b1010)
(test-call "14" (lambda (a b) (logior a b)) #b1100 #b1010)
(test-call "6" (lambda (a b) (logxor a b)) #b1100 #b1010)
;; FIXME: logsub not accessible from scheme
;;(test-call "4" (lambda (a b) (logsub a b)) #b1100 #b1010)

(test-call "0" (lambda (x n) (ash x n)) 1 -2)
(test-call "-1" (lambda (x n) (ash x n)) -1 -1)
(test-call "16" (lambda (x n) (ash x n)) 32 -1)
(test-call "0" (lambda (x n) (ash x n)) 32 -64)

(test-call "4" (lambda (x n) (ash x n)) 1 2)
(test-call "-2" (lambda (x n) (ash x n)) -1 1)
(test-call "64" (lambda (x n) (ash x n)) 32 1)

(test-call "-2" (lambda (x) (ash x 1)) -1)
(test-call "-1" (lambda (x) (ash x -1)) -1)

(test-call "18446744073709551616" (lambda (x) (ash x 64)) 1)
(test-call "0" (lambda (x one) (ash (+ x one) -64)) 536870911 1)

(test-end* "test-bitwise")
