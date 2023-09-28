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
;;; Floating point number tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-flonums")

;; Flonum addition
(test-call "84.0" (lambda (a) (+ (inexact a) (inexact a))) 42)
(test-call "84.0" (lambda (a) (+ (inexact a) a)) 42)
(test-call "536870954.0" (lambda (a b c) (+ (inexact a) (+ b c))) 42 536870911 1)
(test-call "84.0" (lambda (a) (+ a (inexact a))) 42)
(test-call "536870954.0" (lambda (a b c) (+ (+ a b) (inexact c))) 536870911 1 42)

;; Flonum subtraction
(test-call "0.0" (lambda (a) (- (inexact a) (inexact a))) 42)
(test-call "0.0" (lambda (a) (- (inexact a) a)) 42)
(test-call "-536870870.0" (lambda (a b c) (- (inexact a) (+ b c))) 42 536870911 1)
(test-call "0.0" (lambda (a) (- a (inexact a))) 42)
(test-call "536870870.0" (lambda (a b c) (- (+ a b) (inexact c))) 536870911 1 42)

;; Flonum multiplication
(test-call "1764.0" (lambda (a) (* (inexact a) (inexact a))) 42)
(test-call "1764.0" (lambda (a) (* (inexact a) a)) 42)
(test-call "22548578304.0" (lambda (a b c) (* (inexact a) (+ b c))) 42 536870911 1)
(test-call "1764.0" (lambda (a) (* a (inexact a))) 42)
(test-call "22548578304.0" (lambda (a b c) (* (+ a b) (inexact c))) 536870911 1 42)

;; Flonum division
(test-call "5.25"
           (lambda (a b) (/ (inexact a) (inexact b)))
           42 8)
(test-call "0.5" (lambda (a b) (/ (inexact a) b)) 1 2)
(test-call "1.0"
           (lambda (a b) (/ (inexact (+ a b)) (+ a b)))
           536870911 1)
(test-call "0.5" (lambda (a b) (/ a (inexact b))) 1 2)
(test-call "107374182.4"
           (lambda (a b c) (/ (+ a b) (inexact c)))
           536870911 1 5)

(test-call "1.0" (lambda (a) (inexact a)) 1)
(test-call "536870912.0" (lambda (a b) (inexact (+ a b))) 536870911 1)
(test-call "+inf.0" (lambda (a b) (/ (inexact a) (inexact b))) 1 0)
(test-call "0.5" (lambda (a b) (inexact (/ a b))) 1 2)

;; Square root and trigonometry
(test-call "2.0" (lambda (a) (sqrt a)) 4)
(test-call "0.0" (lambda (a) (sin a)) 0)
(test-call "1.0" (lambda (a) (cos a)) 0)
(test-call "0.0" (lambda (a) (tan a)) 0)
(test-call "0.0" (lambda (a) (asin a)) 0)
(test-call "0.0" (lambda (a) (acos a)) 1)
(test-call "0.0" (lambda (a) (atan a)) 0)
(test-call "0.0" (lambda (a b) (atan a b)) 0 1)

(test-call "#t" (lambda (a b) (= a (inexact a))) 23 23)
(test-call "#t" (lambda (a b c) (<= a (inexact (/ b c)))) 23 235 10)

;; comparisons with infinities and NaNs
(test-call "#t" (lambda (a b) (= a b)) +inf.0 +inf.0)
(test-call "#t" (lambda (a b) (= a b)) -inf.0 -inf.0)
(test-call "#f" (lambda (a b) (= a b)) +inf.0 -inf.0)
(test-call "#f" (lambda (a b) (= a b)) +nan.0 +nan.0)
(test-call "#f" (lambda (a b) (= a b)) +inf.0 1.0)
(test-call "#f" (lambda (a b) (= a b)) -inf.0 1.0)
(test-call "#f" (lambda (a b) (= a b)) +nan.0 1.0)

(test-call "#f" (lambda (a b) (< a b)) +inf.0 +inf.0)
(test-call "#f" (lambda (a b) (< a b)) -inf.0 -inf.0)
(test-call "#f" (lambda (a b) (< a b)) +inf.0 -inf.0)
(test-call "#f" (lambda (a b) (< a b)) +nan.0 +nan.0)
(test-call "#f" (lambda (a b) (< a b)) +inf.0 1.0)
(test-call "#t" (lambda (a b) (< a b)) -inf.0 1.0)
(test-call "#f" (lambda (a b) (< a b)) +nan.0 1.0)

(test-call "#f" (lambda (a b) (< a b)) +inf.0 +inf.0)
(test-call "#f" (lambda (a b) (< a b)) -inf.0 -inf.0)
(test-call "#t" (lambda (a b) (< a b)) -inf.0 +inf.0)
(test-call "#f" (lambda (a b) (< a b)) +nan.0 +nan.0)
(test-call "#t" (lambda (a b) (< a b)) 1.0 +inf.0)
(test-call "#f" (lambda (a b) (< a b)) 1.0 -inf.0)
(test-call "#f" (lambda (a b) (< a b)) 1.0 +nan.0)

(test-call "#t" (lambda (a b) (<= a b)) +inf.0 +inf.0)
(test-call "#t" (lambda (a b) (<= a b)) -inf.0 -inf.0)
(test-call "#f" (lambda (a b) (<= a b)) +inf.0 -inf.0)
(test-call "#f" (lambda (a b) (<= a b)) +nan.0 +nan.0)
(test-call "#f" (lambda (a b) (<= a b)) +inf.0 1.0)
(test-call "#t" (lambda (a b) (<= a b)) -inf.0 1.0)
(test-call "#f" (lambda (a b) (<= a b)) +nan.0 1.0)

(test-call "#t" (lambda (a b) (<= a b)) +inf.0 +inf.0)
(test-call "#t" (lambda (a b) (<= a b)) -inf.0 -inf.0)
(test-call "#t" (lambda (a b) (<= a b)) -inf.0 +inf.0)
(test-call "#f" (lambda (a b) (<= a b)) +nan.0 +nan.0)
(test-call "#t" (lambda (a b) (<= a b)) 1.0 +inf.0)
(test-call "#f" (lambda (a b) (<= a b)) 1.0 -inf.0)
(test-call "#f" (lambda (a b) (<= a b)) 1.0 +nan.0)

;;; abs, floor and ceiling
(test-call "0.5" (lambda (a b) (abs (inexact (/ a b)))) 1 2)
(test-call "0.5" (lambda (a b) (abs (inexact (/ a b)))) -1 2)
(test-call "0.0" (lambda (a b) (floor (inexact (/ a b)))) 1 2)
(test-call "-1.0" (lambda (a b) (floor (inexact (/ a b)))) -1 2)
(test-call "1.0" (lambda (a b) (ceiling (inexact (/ a b)))) 1 2)
(test-call "-0.0" (lambda (a b) (ceiling (inexact (/ a b)))) -1 2)

(test-end* "test-flonums")
