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
;;; Hash table tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-hash-tables")

(test-call "#t" (lambda () (hashtable? (make-eq-hashtable))))
(test-call "#t" (lambda () (hashtable? (make-eq-hashtable 42))))

;; Unimplemented:
;; - make-eqv-hashtable
;; - make-hashtable

(test-call "#f" (lambda () (hashtable? 42)))

(test-call "b" (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-ref ht 'a 'b))))

(test-call "#<unspecified>" (lambda ()
                              (let ((ht (make-eq-hashtable)))
                                (hashtable-set! ht 'a 'b))))

(test-call "b" (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-ref ht 'a #f))))

(test-call "#f" (lambda ()
                  (let ((ht (make-eq-hashtable)))
                    (hashtable-set! ht 'x 'y)
                    (hashtable-ref ht 'a #f))))

(test-call "b" (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-ref ht 'a 'b))))

(test-call "0" (lambda ()
                 (hashtable-size (make-eq-hashtable))))

(test-call "1" (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-size ht))))

(test-call "2" (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-set! ht 'c 'd)
                   (hashtable-size ht))))

(test-call "(#f 0)" (lambda ()
                      (let ((ht (make-eq-hashtable)))
                        (hashtable-set! ht 'a 'b)
                        (hashtable-delete! ht 'a)
                        (list (hashtable-ref ht 'a #f)
                              (hashtable-size ht)))))

(test-call "(b 1)" (lambda ()
                      (let ((ht (make-eq-hashtable)))
                        (hashtable-set! ht 'a 'b)
                        (hashtable-delete! ht 'c)
                        (list (hashtable-ref ht 'a #f)
                              (hashtable-size ht)))))

(test-call "#f" (lambda ()
                  (let ((ht (make-eq-hashtable)))
                    (hashtable-contains? ht 'a))))

(test-call "#t" (lambda ()
                  (let ((ht (make-eq-hashtable)))
                    (hashtable-set! ht 'a 'b)
                    (hashtable-contains? ht 'a))))

(test-call "1" (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-update! ht 'a 1+ 0)
                   (hashtable-ref ht 'a #f))))

(test-call "2" (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 1)
                   (hashtable-update! ht 'a 1+ 0)
                   (hashtable-ref ht 'a #f))))

(test-call "(2 b d)"
           (lambda ()
             (let ((ht (make-eq-hashtable)))
               (hashtable-set! ht 'a 'b)
               (hashtable-set! ht 'c 'd)
               (let ((ht* (hashtable-copy ht)))
                 (list (hashtable-size ht*)
                       (hashtable-ref ht* 'a #f)
                       (hashtable-ref ht* 'c #f))))))

(test-call "0" (lambda ()
                 (let ((ht (make-eq-hashtable)))
                   (hashtable-set! ht 'a 'b)
                   (hashtable-clear! ht)
                   (hashtable-size ht))))

(test-call "#()" (lambda ()
                   (hashtable-keys (make-eq-hashtable))))

(test-call "#(a)" (lambda ()
                    (let ((ht (make-eq-hashtable)))
                      (hashtable-set! ht 'a 'b)
                      (hashtable-keys ht))))

(test-call "#()" (lambda ()
                   (hashtable-entries (make-eq-hashtable))))

(test-call "#(b)" (lambda ()
                    (let ((ht (make-eq-hashtable)))
                      (hashtable-set! ht 'a 'b)
                      (hashtable-entries ht))))

(test-call "#t" (lambda ()
                  (eq? eq? (hashtable-equivalence-function
                            (make-eq-hashtable)))))

(test-call "#t" (lambda ()
                  (eq? %hashq (hashtable-hash-function
                               (make-eq-hashtable)))))

(test-call "#t" (lambda ()
                  (hashtable-mutable? (make-eq-hashtable))))

;; Unimplemented:
;; - equal-hash
;; - string-hash
;; - string-ci-hash
;; - symbol-hash

(test-end* "test-hash-tables")
