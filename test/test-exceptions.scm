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
;;; Exception tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-exceptions")

(test-call "79" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 69)))))
(test-call "52" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise-continuable 69))))))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise-continuable 69)))
                   #:unwind? #t)))
(test-call "69" (lambda ()
                  (with-exception-handler
                   (lambda (exn) exn)
                   (lambda () (+ 10 (raise-continuable 69)))
                   #:unwind? #t)))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise 69)))
                   #:unwind? #t)))
(test-call "69" (lambda ()
                  (with-exception-handler
                   (lambda (exn) exn)
                   (lambda () (+ 10 (raise 69)))
                   #:unwind? #t)))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (error "what"))
                   #:unwind? #t)))

(test-call "#(#t \"hey\" (ho))"
           (lambda (message irritants)
             (let ((exn (make-compound-exception
                         (list
                          (make-exception-with-message message)
                          (make-exception-with-irritants irritants)))))
               (vector (error-object? exn)
                       (error-object-message exn)
                       (error-object-irritants exn))))
           "hey"
           '(ho))

(test-call "42"
           (lambda ()
             (guard (condition
                     ((assq 'a condition) => cdr)
                     ((assq 'b condition)))
               (raise (list (cons 'a 42))))))

(test-call "(b . 23)"
           (lambda ()
             (guard (condition
                     ((assq 'a condition) => cdr)
                     ((assq 'b condition)))
               (raise (list (cons 'b 23))))))

(test-end* "test-exceptions")
