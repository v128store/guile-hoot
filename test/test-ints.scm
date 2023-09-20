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
;;; Numeric operation tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-ints")

;;; Fixnum overflow
;; add and sub
(test-call "536870912" (lambda (a b) (+ a b)) 536870911 1)
(test-call "-536870913" (lambda (a b) (- a b)) -536870912 1)
;; add/immediate and sub/immediate
(test-call "536870912" (lambda (a) (1+ a)) 536870911)
(test-call "-536870913" (lambda (a) (1- a)) -536870912)
(test-call "536870953" (lambda (a) (+ a 42)) 536870911)
(test-call "-536870954" (lambda (a) (- a 42)) -536870912)
;; mul
(test-call "391" (lambda (a b) (* a b)) 17 23)
(test-call "536895241" (lambda (a) (* a a)) 23171)

;; Construct two bignums and add them (vs. previous tests producing
;; bignum sums from fixnum inputs)
(test-call "1073741824" (lambda (a b) (+ (+ a b) (+ a b))) 536870911 1)
(test-call "536870954" (lambda (a b c) (+ (+ a b) c)) 536870911 1 42)
(test-call "536870954" (lambda (a b c) (+ c (+ a b))) 536870911 1 42)

(test-call "0" (lambda (a b) (- (+ a b) (+ a b))) 536870911 1)
(test-call "536870870" (lambda (a b c) (- (+ a b) c)) 536870911 1 42)
(test-call "-536870870" (lambda (a b c) (- c (+ a b))) 536870911 1 42)

(test-call "1073741824" (lambda (a b c) (* (+ a b) c)) 536870911 1 2)
(test-call "1073741824" (lambda (a b c) (* c (+ a b))) 536870911 1 2)
(test-call "288230376151711744" (lambda (a b) (* (+ a b) (+ a b))) 536870911 1)

;; Integer division
;; Quotient
(test-call "12" (lambda (a b) (quotient a b)) 123 10)
(test-call "-12" (lambda (a b) (quotient a b)) 123 -10)
(test-call "-12" (lambda (a b) (quotient a b)) -123 10)
(test-call "12" (lambda (a b) (quotient a b)) -123 -10)

(test-call "53687091" (lambda (a b c) (quotient (+ a b) c)) 536870911 1 10)
(test-call "-53687091" (lambda (a b c) (quotient (+ a b) c)) 536870911 1 -10)
(test-call "-53687091" (lambda (a b c) (quotient (+ a b) c)) -536870911 -1 10)
(test-call "53687091" (lambda (a b c) (quotient (+ a b) c)) -536870911 -1 -10)

(test-call "0" (lambda (a b c) (quotient a (+ b c))) 10 536870911 1)
(test-call "0" (lambda (a b c) (quotient a (+ b c))) 10 -536870911 -1)
(test-call "0" (lambda (a b c) (quotient a (+ b c))) -10 536870911 1)
(test-call "0" (lambda (a b c) (quotient a (+ b c))) -10 -536870911 -1)

(test-call "1"
           (lambda (a b c d) (quotient (+ a b) (+ c d)))
           536870911 2 536870911 1)
(test-call "-1"
           (lambda (a b c d) (quotient (+ a b) (+ c d)))
           536870911 2 -536870911 -1)
(test-call "-1"
           (lambda (a b c d) (quotient (+ a b) (+ c d)))
           -536870911 -2 536870911 1)
(test-call "1"
           (lambda (a b c d) (quotient (+ a b) (+ c d)))
           -536870911 -2 -536870911 -1)

;; Remainder
(test-call "3" (lambda (a b) (remainder a b)) 123 10)
(test-call "3" (lambda (a b) (remainder a b)) 123 -10)
(test-call "-3" (lambda (a b) (remainder a b)) -123 10)
(test-call "-3" (lambda (a b) (remainder a b)) -123 -10)

(test-call "2" (lambda (a b c) (remainder (+ a b) c)) 536870911 1 10)
(test-call "2" (lambda (a b c) (remainder (+ a b) c)) 536870911 1 -10)
(test-call "-2" (lambda (a b c) (remainder (+ a b) c)) -536870911 -1 10)
(test-call "-2" (lambda (a b c) (remainder (+ a b) c)) -536870911 -1 -10)

(test-call "10" (lambda (a b c) (remainder a (+ b c))) 10 536870911 1)
(test-call "10" (lambda (a b c) (remainder a (+ b c))) 10 -536870911 -1)
(test-call "-10" (lambda (a b c) (remainder a (+ b c))) -10 536870911 1)
(test-call "-10" (lambda (a b c) (remainder a (+ b c))) -10 -536870911 -1)

(test-call "1"
           (lambda (a b c d) (remainder (+ a b) (+ c d)))
           536870911 2 536870911 1)
(test-call "1"
           (lambda (a b c d) (remainder (+ a b) (+ c d)))
           536870911 2 -536870911 -1)
(test-call "-1"
           (lambda (a b c d) (remainder (+ a b) (+ c d)))
           -536870911 -2 536870911 1)
(test-call "-1"
           (lambda (a b c d) (remainder (+ a b) (+ c d)))
           -536870911 -2 -536870911 -1)

;; Modulo
(test-call "3" (lambda (a b) (modulo a b)) 123 10)
(test-call "-7" (lambda (a b) (modulo a b)) 123 -10)
(test-call "7" (lambda (a b) (modulo a b)) -123 10)
(test-call "-3" (lambda (a b) (modulo a b)) -123 -10)

(test-call "2" (lambda (a b c) (modulo (+ a b) c)) 536870911 1 10)
(test-call "-8" (lambda (a b c) (modulo (+ a b) c)) 536870911 1 -10)
(test-call "8" (lambda (a b c) (modulo (+ a b) c)) -536870911 -1 10)
(test-call "-2" (lambda (a b c) (modulo (+ a b) c)) -536870911 -1 -10)

(test-call "10" (lambda (a b c) (modulo a (+ b c))) 10 536870911 1)
(test-call "-536870902" (lambda (a b c) (modulo a (+ b c))) 10 -536870911 -1)
(test-call "536870902" (lambda (a b c) (modulo a (+ b c))) -10 536870911 1)
(test-call "-10" (lambda (a b c) (modulo a (+ b c))) -10 -536870911 -1)

(test-call "1"
           (lambda (a b c d) (modulo (+ a b) (+ c d)))
           536870911 2 536870911 1)
(test-call "-536870911"
           (lambda (a b c d) (modulo (+ a b) (+ c d)))
           536870911 2 -536870911 -1)
(test-call "536870911"
           (lambda (a b c d) (modulo (+ a b) (+ c d)))
           -536870911 -2 536870911 1)
(test-call "-1"
           (lambda (a b c d) (modulo (+ a b) (+ c d)))
           -536870911 -2 -536870911 -1)

(test-call "3" (lambda (x y) (quotient x y)) 10 3)
(test-call "-3" (lambda (x y) (quotient x y)) -10 3)
(test-call "1" (lambda (x y) (remainder x y)) 10 3)
(test-call "-1" (lambda (x y) (remainder x y)) -10 3)
(test-call "1" (lambda (x y) (modulo x y)) 10 3)
(test-call "2" (lambda (x y) (modulo x y)) -10 3)

;;; abs, floor and ceiling
(test-call "42" (lambda (a) (abs a)) 42)
(test-call "42" (lambda (a) (abs a)) -42)
(test-call "536870912" (lambda (a b) (abs (+ a b))) 536870911 1)
(test-call "536870913" (lambda (a b) (abs (+ a b))) -536870911 -2)
(test-call "23" (lambda (a) (floor a)) 23)
(test-call "23" (lambda (a) (ceiling a)) 23)
(test-call "536870912" (lambda (a b) (floor (+ a b))) 536870911 1)
(test-call "536870912" (lambda (a b) (ceiling (+ a b))) 536870911 1)
(test-call "-536870913" (lambda (a b) (floor (+ a b))) -536870911 -2)
(test-call "-536870913" (lambda (a b) (ceiling (+ a b))) -536870911 -2)

(test-end* "test-ints")
