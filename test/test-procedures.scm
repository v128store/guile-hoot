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
;;; Procedure tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-procedures")

(test-call "()" (lambda args args))
(test-call "(1)" (lambda args args) 1)
(test-call "(1 2)" (lambda args args) 1 2)
(test-call "(1 2 3)" (lambda args args) 1 2 3)
(test-call "(1 2 3 4)" (lambda args args) 1 2 3 4)
(test-call "(1 2 3 4 5)" (lambda args args) 1 2 3 4 5)
(test-call "(1 2 3 4 5 6)" (lambda args args) 1 2 3 4 5 6)
(test-call "(1 2 3 4 5 6 7)" (lambda args args) 1 2 3 4 5 6 7)
(test-call "(1 2 3 4 5 6 7 8)" (lambda args args) 1 2 3 4 5 6 7 8)
(test-call "(1 2 3 4 5 6 7 8 9)" (lambda args args) 1 2 3 4 5 6 7 8 9)
(test-call "(1 2 3 4 5 6 7 8 9 10)" (lambda args args) 1 2 3 4 5 6 7 8 9 10)

(test-call "(1 2)" (lambda (a . args) (cons* a args)) 1 2)
(test-call "(1 2 3)" (lambda (a b . args) (cons* a b args)) 1 2 3)
(test-call "(1 2 3 4)" (lambda (a b c . args) (cons* a b c args)) 1 2 3 4)
(test-call "(1 2 3 4 5)" (lambda (a b c d . args) (cons* a b c d args)) 1 2 3 4 5)
(test-call "(1 2 3 4 5 6 7 8 9 10 11 12)"
           (lambda (a b c d e f g h i j . args)
             (cons* a b c d e f g h i j args))
           1 2 3 4 5 6 7 8 9 10 11 12)

(test-call "#f" (lambda* (#:optional a) a))
(test-call "(42 69)" (lambda* (#:optional (a 42) (b 69)) (list a b)))
(test-call "(10 20)" (lambda* (#:optional (a 42) (b 69)) (list a b)) 10 20)

(test-call "(1 #f ())" (lambda* (a #:optional b . rest) (list a b rest)) 1)

(test-call "(1 2 3 4 5 6 7 8 9 10 11 12)"
           (lambda* (a b c d #:optional e f g h i j . args)
             (cons* a b c d e f g h i j args))
           1 2 3 4 5 6 7 8 9 10 11 12)

(test-call "(1 2 3 4 5 6 #f #f #f #f)"
           (lambda* (a b c d #:optional e f g h i j . args)
             (cons* a b c d e f g h i j args))
           1 2 3 4 5 6)

(test-call "20" (lambda (f . args) (apply f args)) (lambda (x y) (+ x y)) 12 8)
(test-call "12\n8" (lambda (f . args) (apply f args)) values 12 8)

(test-call "52" (lambda (f) ((f 42))) (lambda (n) (lambda () (+ n 10))))

(test-call "120" (lambda (n)
                   (let fac ((n n))
                     (if (eq? n 0)
                         1
                         (* n (fac (1- n))))))
           5)

(test-call "42" (case-lambda ((a) a) ((a b) (+ a b))) 42)
(test-call "52" (case-lambda ((a) a) ((a b) (+ a b))) 42 10)

;; (test-call "9227465" (lambda (n)
;;                        (let fib ((n n))
;;                          (if (<= n 1)
;;                              1
;;                              (+ (fib (- n 1)) (fib (- n 2))))))
;;            34)

;; (test-call "1000000" (lambda ()
;;                        (let lp ((n 0))
;;                          (if (< n #e1e6)
;;                              (lp (1+ n))
;;                              n))))

(test-end* "test-procedures")
