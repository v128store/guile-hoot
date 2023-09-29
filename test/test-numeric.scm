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
;;; Miscellaneous numeric tower tests, primarily for mixed-type
;;; operations.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-numeric")

;; Flonum<->fraction comparison
;; ...with infinities and NaN
(test-call "#t" (lambda (a b) (< a b)) 1/2 +inf.0)
(test-call "#f" (lambda (a b) (< a b)) 1/2 -inf.0)
(test-call "#f" (lambda (a b) (< a b)) +nan.0 1/2)

(test-call "#f" (lambda (a b) (< a b)) +inf.0 1/2)
(test-call "#t" (lambda (a b) (< a b)) -inf.0 1/2)
(test-call "#f" (lambda (a b) (< a b)) 1/2 +nan.0)

(test-call "#t" (lambda (a b) (<= a b)) 1/2 +inf.0)
(test-call "#f" (lambda (a b) (<= a b)) 1/2 -inf.0)
(test-call "#f" (lambda (a b) (<= a b)) +nan.0 1/2)

(test-call "#f" (lambda (a b) (<= a b)) +inf.0 1/2)
(test-call "#t" (lambda (a b) (<= a b)) -inf.0 1/2)
(test-call "#f" (lambda (a b) (<= a b)) 1/2 +nan.0)

(test-call "#f" (lambda (a b) (= a b)) 1/2 +inf.0)
(test-call "#f" (lambda (a b) (= a b)) 1/2 -inf.0)
(test-call "#f" (lambda (a b) (= a b)) +nan.0 1/2)

(test-call "#f" (lambda (a b) (= a b)) +inf.0 1/2)
(test-call "#f" (lambda (a b) (= a b)) -inf.0 1/2)
(test-call "#f" (lambda (a b) (= a b)) 1/2 +nan.0)

;; ...with ordinary flonums
(test-call "#t" (lambda (a b) (< a b)) 1/2 1.0)
(test-call "#f" (lambda (a b) (< a b)) 1/2 -1.0)
(test-call "#f" (lambda (a b) (< a b)) 1.0 1/2)
(test-call "#t" (lambda (a b) (< a b)) -1.0 1/2)

(test-call "#t" (lambda (a b) (<= a b)) 1/2 1.0)
(test-call "#f" (lambda (a b) (<= a b)) 1/2 -1.0)
(test-call "#f" (lambda (a b) (<= a b)) 1.0 1/2)
(test-call "#t" (lambda (a b) (<= a b)) -1.0 1/2)

(test-call "#f" (lambda (a b) (= a b)) 1/2 1.0)
(test-call "#f" (lambda (a b) (= a b)) 1/2 -1.0)
(test-call "#f" (lambda (a b) (= a b)) 1.0 1/2)
(test-call "#f" (lambda (a b) (= a b)) -1.0 1/2)

(test-call "#f" (lambda (a b) (= a b) 1/2 0.5))
(test-call "#f" (lambda (a b) (= a b) 0.5 1/2))
(test-call "#t" (lambda (a b) (<= a b) 1/2 0.5))
(test-call "#t" (lambda (a b) (<= a b) 0.5 1/2))
(test-call "#t" (lambda (a b) (= a b) 0.5 1/2))
(test-call "#t" (lambda (a b) (= a b) 0.5 1/2))

(test-end "test-numeric")
