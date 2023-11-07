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

(test-begin "test-records")

(test-call "42" (lambda ()
                  (define-record-type q (make-q a) q? (a q-a))
                  (q-a (make-q 42))))
(test-call "42" (lambda ()
                  (define-record-type q (make-q a) q? (a q-a set-q-a!))
                  (define x (make-q 10))
                  (set-q-a! x 42)
                  (q-a x)))

(test-call "42" (lambda (args)
                  (match args
                    ((make-q q-a set-q-a!)
                     (define x (make-q 10))
                     (set-q-a! x 42)
                     (q-a x))))
           (let ()
             (define-record-type q (make-q a) q? (a q-a set-q-a!))
             (list make-q q-a set-q-a!)))

(test-call "42" (lambda (args)
                  (match args
                    ((make-q q-a set-q-a!)
                     (define x (make-q 10))
                     (set-q-a! x 42)
                     (q-a x))))
           (let ()
             (define-record-type q (make-q b a) q? (b q-b set-q-b!) (a q-a set-q-a!))
             (list (lambda (a) (make-q #f a)) q-a set-q-a!)))

(test-call "\"#<q>\"" (lambda ()
                    (define-record-type q (make-q) q?)
                    (define (call-with-output-string f)
                      (let ((p (open-output-string)))
                        (f p)
                        (get-output-string p)))
                    (call-with-output-string
                     (lambda (port)
                       (write (make-q) port)))))

(test-call "\"#<q a: 42>\"" (lambda ()
                    (define-record-type q (make-q a) q? (a q-a))
                    (define (call-with-output-string f)
                      (let ((p (open-output-string)))
                        (f p)
                        (get-output-string p)))
                    (call-with-output-string
                     (lambda (port)
                       (write (make-q 42) port)))))

(test-call "\"#<q>\"" (lambda ()
                    (define-record-type q
                      #:opaque? #t
                      (make-q a) q? (a q-a))
                    (define (call-with-output-string f)
                      (let ((p (open-output-string)))
                        (f p)
                        (get-output-string p)))
                    (call-with-output-string
                     (lambda (port)
                       (write (make-q 42) port)))))

(test-call "\"#<big a: 1 b: 2 c: 3 d: 4 e: 5 f: 6 g: 7 h: 8 i: 9 j: 10>\""
           (lambda ()
             (define-record-type big
               (make-big a b c d e f g h i j)
               big?
               (a big-a)
               (b big-b)
               (c big-c)
               (d big-d)
               (e big-e)
               (f big-f)
               (g big-g)
               (h big-h)
               (i big-i)
               (j big-j))
             (define (call-with-output-string f)
               (let ((p (open-output-string)))
                 (f p)
                 (get-output-string p)))
             (call-with-output-string
              (lambda (port)
                (write (make-big 1 2 3 4 5 6 7 8 9 10) port)))))

(test-call "\"#<marmot 42>\""
           (lambda ()
             (define-record-type q
               #:printer (lambda (x port)
                           (write-string "#<marmot " port)
                           (write (q-a x) port)
                           (write-string ">" port))
               (make-q a)
               q?
               (a q-a))
             (define (call-with-output-string f)
               (let ((p (open-output-string)))
                 (f p)
                 (get-output-string p)))
             (call-with-output-string
              (lambda (port)
                (write (make-q 42) port)))))

(test-call "#t"
           (lambda ()
             (define-record-type q (make-q a) q? (a q-a))
             (let ((a (make-q 42))
                   (b (make-q 42))
                   (c (make-q 69)))
               (and (eq? a a) (eq? b b) (eq? c c)
                    (eqv? a a) (eqv? b b) (eqv? c c)
                    (equal? a a) (equal? b b) (equal? c c)
                    (not (eqv? a b))
                    (not (eqv? b c))
                    (not (eqv? a c))
                    (equal? a b)
                    (not (equal? b c))
                    (not (equal? a c))))))

(test-call "#t"
           (lambda ()
             (define-record-type q #:opaque? #t (make-q a) q? (a q-a))
             (let ((a (make-q 42))
                   (b (make-q 42))
                   (c (make-q 69)))
               (and (eq? a a) (eq? b b) (eq? c c)
                    (eqv? a a) (eqv? b b) (eqv? c c)
                    (equal? a a) (equal? b b) (equal? c c)
                    (not (eqv? a b))
                    (not (eqv? b c))
                    (not (eqv? a c))
                    (not (equal? a b))
                    (not (equal? b c))
                    (not (equal? a c))))))

(test-call "#t"
           (lambda ()
             (define-record-type x #:extensible? #t (make-x a) x? (a x-a))
             (define-record-type y #:extensible? #t #:parent x (make-y a b) y? (b y-b))
             (define-record-type z #:parent y (make-z a b c) z? (c z-c))
             (let ((q (make-y 42 69))
                   (r (make-z 42 69 420)))
               (and (x? q)
                    (y? q)
                    (not (z? q))
                    (eq? (x-a q) 42)
                    (eq? (y-b q) 69)
                    (x? r)
                    (y? r)
                    (z? r)
                    (eq? (x-a r) 42)
                    (eq? (y-b r) 69)
                    (eq? (z-c r) 420)))))

(test-end* "test-records")
