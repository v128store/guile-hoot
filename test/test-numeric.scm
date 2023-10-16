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

;; exact
(test-call "1" (lambda (a) (exact a)) 1)
(test-call "1/2" (lambda (a) (exact a)) 1/2)
(test-call "0" (lambda (a) (exact a)) 0.0)
(test-call "0" (lambda (a) (exact a)) -0.0)
(test-call "1/2" (lambda (a) (exact a)) 0.5)
(test-call "-1/2" (lambda (a) (exact a)) -0.5)
(test-call "9999" (lambda (a) (exact a)) 9999.0)
(test-call "-9999" (lambda (a) (exact a)) -9999.0)

;; numeric equivalence
(test-call "#t" (lambda (a b) (eqv? a b)) 1 1)
(test-call "#f" (lambda (a b) (eqv? a b)) 1 1.0)
(test-call "#t" (lambda (a b) (eqv? a (+ b b))) 1/2 1/4)
(test-call "#t" (lambda (a b) (eqv? +nan.0 +nan.0)))
(test-call "#t" (lambda (a b) (eqv? +inf.0 +inf.0)))
(test-call "#t" (lambda (a b) (eqv? -inf.0 -inf.0)))

(test-call "#t" (lambda (a b) (equal? a b)) 1 1)
(test-call "#f" (lambda (a b) (equal? a b)) 1 1.0)
(test-call "#t" (lambda (a b) (equal? a (+ b b))) 1/2 1/4)
(test-call "#t" (lambda (a b) (equal? +nan.0 +nan.0)))
(test-call "#t" (lambda (a b) (equal? +inf.0 +inf.0)))
(test-call "#t" (lambda (a b) (equal? -inf.0 -inf.0)))

;; numerator and denominator
(test-call "42" (lambda (a) (numerator a)) 42)
(test-call "1" (lambda (a) (denominator a)) 42)
(test-call "3" (lambda (a) (numerator a)) 6/4)
(test-call "2" (lambda (a) (denominator a)) 6/4)
(test-call "2.0" (lambda (a) (denominator (inexact a))) 6/4)

;; log and exp
(test-call "0.0" (lambda (a) (log a)) 1)
(test-call "2.0" (lambda (a b) (log a b)) 100 10)
(test-call "1.0" (lambda (a) (exp (log a))) 1)
(test-call "0.5" (lambda (a) (exp (log a))) 1/2)

;; expt
(test-call "1" (lambda (a b) (expt a b)) 0 0)
(test-call "1.0" (lambda (a b) (expt a b)) 0 0.0)
(test-call "0" (lambda (a b) (expt a b)) 0 42)
(test-call "0.0" (lambda (a b) (expt a b)) 0 42.0)
(test-call "+nan.0" (lambda (a b) (expt a b)) 0 -1)
(test-call "1.0" (lambda (a b) (expt a b)) 0.0 0)
(test-call "1.0" (lambda (a b) (expt a b)) 0.0 0.0)
(test-call "0.0" (lambda (a b) (expt a b)) 0.0 42)
(test-call "0.0" (lambda (a b) (expt a b)) 0.0 42.0)
(test-call "+nan.0" (lambda (a b) (expt a b)) 0.0 -1)
(test-call "1" (lambda (a b) (expt a b)) 1 0)
(test-call "1" (lambda (a b) (expt a b)) 1 1)
(test-call "1" (lambda (a b) (expt a b)) 1 2)
(test-call "1" (lambda (a b) (expt a b)) 1 3)
(test-call "1" (lambda (a b) (expt a b)) 2 0)
(test-call "2" (lambda (a b) (expt a b)) 2 1)
(test-call "4" (lambda (a b) (expt a b)) 2 2)
(test-call "8" (lambda (a b) (expt a b)) 2 3)
(test-call "1" (lambda (a b) (expt a b)) 1/2 0)
(test-call "1/2" (lambda (a b) (expt a b)) 1/2 1)
(test-call "1/4" (lambda (a b) (expt a b)) 1/2 2)
(test-call "1/8" (lambda (a b) (expt a b)) 1/2 3)
(test-call "2.0" (lambda (a b) (expt a b)) 4 1/2)
(test-call "2.0" (lambda (a b) (expt a b)) 4 0.5)
(test-call "2.0" (lambda (a b) (expt a b)) 4.0 1/2)
(test-call "2.0" (lambda (a b) (expt a b)) 4.0 0.5)
(test-call "+inf.0" (lambda (a b) (expt a b)) +inf.0 +inf.0)
(test-call "0.0" (lambda (a b) (expt a b)) +inf.0 -inf.0)

;; truncating division
(test-call "(2 1)"
           (lambda (a b) (call-with-values
                             (lambda () (truncate/ a b))
                           (lambda x x)))
           5 2)
(test-call "(-2 -1)"
           (lambda (a b) (call-with-values
                             (lambda () (truncate/ a b))
                           (lambda x x)))
           -5 2)
(test-call "(-2 1)"
           (lambda (a b) (call-with-values
                             (lambda () (truncate/ a b))
                           (lambda x x)))
           5 -2)
(test-call "(2 -1)"
           (lambda (a b) (call-with-values
                             (lambda () (truncate/ a b))
                           (lambda x x)))
           -5 -2)
(test-call "(2.0 -1.0)"
           (lambda (a b) (call-with-values
                             (lambda () (truncate/ a b))
                           (lambda x x)))
           -5.0 -2)

(test-call "2" (lambda (a b) (truncate-quotient a b)) 5 2)
(test-call "-2" (lambda (a b) (truncate-quotient a b)) -5 2)
(test-call "-2" (lambda (a b) (truncate-quotient a b)) 5 -2)
(test-call "2" (lambda (a b) (truncate-quotient a b)) -5 -2)
(test-call "2.0" (lambda (a b) (truncate-quotient a b)) -5.0 -2)

(test-call "1" (lambda (a b) (truncate-remainder a b)) 5 2)
(test-call "-1" (lambda (a b) (truncate-remainder a b)) -5 2)
(test-call "1" (lambda (a b) (truncate-remainder a b)) 5 -2)
(test-call "-1" (lambda (a b) (truncate-remainder a b)) -5 -2)
(test-call "-1.0" (lambda (a b) (truncate-remainder a b)) -5.0 -2)

;; flooring division
(test-call "2 1" (lambda (a b) (call-with-values
                                   (lambda () (floor/ a b))
                                 (lambda x x)))
           5 2)
(test-call "-3 1" (lambda (a b) (call-with-values
                                    (lambda () (floor/ a b))
                                  (lambda x x)))
           -5 2)
(test-call "-3 -1" (lambda (a b) (call-with-values
                                     (lambda () (floor/ a b))
                                   (lambda x x)))
           5 -2)
(test-call "2 -1" (lambda (a b) (call-with-values
                                    (lambda () (floor/ a b))
                                  (lambda x x)))
           -5 -2)

(test-call "2" (lambda (a b) (floor-quotient a b)) 5 2)
(test-call "-3" (lambda (a b) (floor-quotient a b)) -5 2)
(test-call "-3" (lambda (a b) (floor-quotient a b)) 5 -2)
(test-call "2" (lambda (a b) (floor-quotient a b)) -5 -2)

(test-call "1" (lambda (a b) (floor-remainder a b)) 5 2)
(test-call "1" (lambda (a b) (floor-remainder a b)) -5 2)
(test-call "-1" (lambda (a b) (floor-remainder a b)) 5 -2)
(test-call "-1" (lambda (a b) (floor-remainder a b)) -5 -2)

;; gcd and lcm
(test-call "4" (lambda (a b) (gcd a b)) 32 -36)
(test-call "0" (lambda () (gcd)))
(test-call "288" (lambda (a b) (lcm a b)) 32 -36)
(test-call "288.0" (lambda (a b) (lcm a b)) 32.0 -36)
(test-call "1" (lambda () (lcm)))

;; rationalize
(test-call "1/3" (lambda (a b) (rationalize a b)) 3/10 1/10)
(test-call "0.2" (lambda (a b) (rationalize a b)) 0.2 1/10)

;; exact-integer-sqrt
(test-call "(0 0)" (lambda (a) (call-with-values
                                   (lambda () (exact-integer-sqrt a))
                                 (lambda x x)))
           0)
(test-call "(1 0)" (lambda (a) (call-with-values
                                   (lambda () (exact-integer-sqrt a))
                                 (lambda x x)))
           1)
(test-call "(1 1)" (lambda (a) (call-with-values
                                   (lambda () (exact-integer-sqrt a))
                                 (lambda x x)))
           2)
(test-call "(1 2)" (lambda (a) (call-with-values
                                   (lambda () (exact-integer-sqrt a))
                                 (lambda x x)))
           3)
(test-call "(2 0)" (lambda (a) (call-with-values
                                   (lambda () (exact-integer-sqrt a))
                                 (lambda x x)))
           4)
(test-call "(2 1)" (lambda (a) (call-with-values
                                   (lambda () (exact-integer-sqrt a))
                                 (lambda x x)))
           5)
(test-call "(4294967296 1)"
           (lambda () (call-with-values
                          (lambda () (exact-integer-sqrt (1+ (expt 2 64))))
                        (lambda x x))))

(test-end "test-numeric")
