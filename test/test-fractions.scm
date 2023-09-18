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
;;; Fraction tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-fractions")

;; Exact fraction arithmetic
(test-call "1/2" (lambda (a b) (/ a b)) 1 2)
(test-call "3/536870912" (lambda (a b c) (/ a (+ b c))) 3 536870911 1)
(test-call "536870912/3" (lambda (a b c) (/ (+ a b) c)) 536870911 1 3)
(test-call "536870912/536870913"
           (lambda (a b) (/ (+ a b) (+ a b b)))
           536870911 1)

(test-call "1/2" (lambda (a b) (* a (/ a b))) 1 2)
(test-call "1" (lambda (a b) (* b (/ a b))) 1 2)
(test-call "1/4" (lambda (a b) (* (/ a b) (/ a b))) 2 4)
(test-call "1" (lambda (a b) (* (/ b a) (/ a b))) 1 2)
;; FIXME:
;; (test-call "1"
;;            (lambda (a b) (* (/ (+ a b) (+ a b)) (/ (+ a b) (+ a b))))
;;            536870911 1)

(test-call "3/2" (lambda (a b c) (+ a (/ b c))) 1 1 2)
(test-call "1073741825/2"
           (lambda (a b c d) (+ (+ a b) (/ c d)))
           536870911 1 1 2)
(test-call "7/6" (lambda (a b c d) (+ (/ a b) (/ c d))) 1 2 2 3)
(test-call "7/6" (lambda (a b c d) (+ (/ a b) (/ c d))) 2 4 4 6)

(test-call "1/2" (lambda (a b c) (- a (/ b c))) 1 1 2)
(test-call "-1/2" (lambda (a b c) (- (/ a b) c)) 1 2 1)
(test-call "-1/2" (lambda (a b c) (- (/ a b) (/ c c))) 1 2 1)
(test-call "1610612735/3"
           (lambda (a b c d) (- (+ a b) (/ c d)))
           536870911 1 1 3)
(test-call "-1610612735/3"
           (lambda (a b c d) (- (/ a b) (+ c d)))
           1 3 536870911 1)
(test-call "-1/6" (lambda (a b c d) (- (/ a b) (/ c d))) 1 2 2 3)
(test-call "-1/2" (lambda (a b) (/ a b)) 1 -2)

(test-call "#t" (lambda (a b c) (< a (/ b c))) 0 1 2)

(test-end* "test-fractions")
