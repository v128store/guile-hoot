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
;;; String tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-write")

(define-syntax test-write
  (syntax-rules ()
    ((_ datum) (test-write (object->string datum) datum))
    ((_ expected-output datum)
     (test-write expected-output expected-output datum)
     )
    ((_ scheme-repr reflect-repr datum)
     (let ((output (string-append scheme-repr reflect-repr)))
       (test-call output
                  (lambda ()
                    (write datum (current-output-port))
                    (flush-output-port (current-output-port))
                    datum))))))

(test-write #f)
(test-write #t)
(test-write #nil)
(test-write '())
(test-write (if #f #f))
(let ((eof-object (lambda () the-eof-object)))
  (test-write (eof-object)))
(test-write 42)
(test-write -42)
(test-write 42.0)
(test-write -42.0)
(test-write #\a)
(test-write '(1 . 2))
(test-write '(1 2))
(test-write "foo")
(test-write 'foo)
(test-write #vu8())
(test-write #vu8(1 2 3))
(test-write #())
(test-write #(1 2 3))
(test-write #*)
(test-write #*110110)
(test-write "#<procedure>" (lambda () 42))
(test-write #:foo)
;; Not yet implemented:
;;   Boxes
;;   Atomic boxes
;;   Weak tables
;;   Fluids
;;   Dynamic states
;;   Syntax
;;   Ports
;;   Structs / records
;;   Parameters

(test-end* "test-write")
