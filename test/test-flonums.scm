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
(test-call "84" (lambda (a) (+ (inexact a) (inexact a))) 42)
(test-call "84" (lambda (a) (+ (inexact a) a)) 42)
(test-call "536870954" (lambda (a b c) (+ (inexact a) (+ b c))) 42 536870911 1)
(test-call "84" (lambda (a) (+ a (inexact a))) 42)
(test-call "536870954" (lambda (a b c) (+ (+ a b) (inexact c))) 536870911 1 42)

;; Flonum subtraction
(test-call "0" (lambda (a) (- (inexact a) (inexact a))) 42)
(test-call "0" (lambda (a) (- (inexact a) a)) 42)
(test-call "-536870870" (lambda (a b c) (- (inexact a) (+ b c))) 42 536870911 1)
(test-call "0" (lambda (a) (- a (inexact a))) 42)
(test-call "536870870" (lambda (a b c) (- (+ a b) (inexact c))) 536870911 1 42)

;; Flonum multiplication
(test-call "1764" (lambda (a) (* (inexact a) (inexact a))) 42)
(test-call "1764" (lambda (a) (* (inexact a) a)) 42)
(test-call "22548578304" (lambda (a b c) (* (inexact a) (+ b c))) 42 536870911 1)
(test-call "1764" (lambda (a) (* a (inexact a))) 42)
(test-call "22548578304" (lambda (a b c) (* (+ a b) (inexact c))) 536870911 1 42)

;; Flonum division
(test-call "5.25"
           (lambda (a b) (/ (inexact a) (inexact b)))
           42 8)
(test-call "0.5" (lambda (a b) (/ (inexact a) b)) 1 2)
(test-call "1"
           (lambda (a b) (/ (inexact (+ a b)) (+ a b)))
           536870911 1)
(test-call "0.5" (lambda (a b) (/ a (inexact b))) 1 2)
(test-call "107374182.4"
           (lambda (a b c) (/ (+ a b) (inexact c)))
           536870911 1 5)

(test-call "1" (lambda (a) (inexact a)) 1)
(test-call "536870912" (lambda (a b) (inexact (+ a b))) 536870911 1)
(test-call "Infinity" (lambda (a b) (/ (inexact a) (inexact b))) 1 0)
(test-call "0.5" (lambda (a b) (inexact (/ a b))) 1 2)

;; Square root and trigonometry
(test-call "2" (lambda (a) (sqrt a)) 4)
(test-call "0" (lambda (a) (sin a)) 0)
(test-call "1" (lambda (a) (cos a)) 0)
(test-call "0" (lambda (a) (tan a)) 0)
(test-call "0" (lambda (a) (asin a)) 0)
(test-call "0" (lambda (a) (acos a)) 1)
(test-call "0" (lambda (a) (atan a)) 0)
(test-call "0" (lambda (a b) (atan a b)) 0 1)

(test-call "true" (lambda (a b) (= a (inexact a))) 23 23)
(test-call "true" (lambda (a b c) (<= a (inexact (/ b c)))) 23 235 10)

(test-end* "test-flonums")