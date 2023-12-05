;;; Copyright (C) 2023 David Thompson
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

(test-begin "test-string-to-number")

(define-syntax test-string->number
  (syntax-rules ()
    ((_ str) (test-string->number str str))
    ((_ expected-output str)
     (test-string->number expected-output expected-output str))
    ((_ scheme-repr reflect-repr str)
     (test-call reflect-repr (lambda (x) (string->number x)) str))))

(test-string->number "42")
(test-string->number "-42")
(test-string->number "42000.0" "42e3")
(test-string->number "0.042" "42e-3")
(test-string->number "42" "#b101010")
(test-string->number "42" "#o52")
(test-string->number "42" "#d42")
(test-string->number "-42" "#d-42")
(test-string->number "42" "#x2a")
(test-string->number "42" "#x2A")
(test-string->number "1/2")
(test-string->number "-1/2")
(test-string->number "+inf.0")
(test-string->number "-inf.0")
(test-string->number "+nan.0")
(test-string->number "+nan.0" "-nan.0")
(test-string->number "6/5" "#e1.2")
(test-string->number "42.0" "#i42")
(test-string->number "255.0" "#i#xff")
(test-string->number "255.0" "#x#iff")
(test-string->number "0.0+1.0i" "+i")
(test-string->number "0.0-1.0i" "-i")
(test-string->number "2.0+1.0i" "2+i")
(test-string->number "1.0-2.0i" "1-2i")
(test-string->number "2" "2@0")
;; Some invalid strings
(test-string->number "#f" "")
(test-string->number "#f" "foo")
(test-string->number "#f" "+foo")
(test-string->number "#f" "-foo")
(test-string->number "#f" "#xfoo")
(test-string->number "#f" "1/foo")
(test-string->number "#f" "foo/2")
(test-string->number "#f" "foo.0")
(test-string->number "#f" "-1/-2")
(test-string->number "#f" "1.0/2")
(test-string->number "#f" "#x1.2")
(test-string->number "#f" "+inf")
(test-string->number "#f" "inf.0")
(test-string->number "#f" "nan.0")

(test-end* "test-string-to-number")
