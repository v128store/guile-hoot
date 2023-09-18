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

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-number-to-string")

(define-syntax test-number->string
  (syntax-rules ()
    ((_ n) (test-number->string (number->string n) n))
    ((_ expected-output n)
     (test-number->string expected-output expected-output n))
    ((_ scheme-repr reflect-repr n)
     (let ((output (string-append scheme-repr reflect-repr)))
       (test-call output
                  (lambda ()
                    (write-string (number->string n) (current-output-port))
                    (flush-output-port (current-output-port))
                    n))))))

(test-number->string 42)

(test-end* "test-number-to-string")
