;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
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
;;; FFI tests.
;;;
;;; Code:

(use-modules (ice-9 binary-ports)
             (ice-9 exceptions)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-64)
             (test utils)
             (hoot reflect)
             (wasm parse))

(test-begin "test-ffi")

(define-syntax-rule (test-ffi name expected source imports)
  (test-equal name
    expected
    (compile-value 'source imports)))

(test-ffi
 "i32 param and result"
 16
 (let ()
   (define-foreign fsquare
     "math" "square"
     i32 -> i32)
   (square 4))
 `(("math" . (("square" . ,(lambda (x) (* x x)))))))

(test-ffi
 "i64 param and result"
 16
 (let ()
   (define-foreign fsquare
     "math" "square"
     i64 -> i64)
   (square 4))
 `(("math" . (("square" . ,(lambda (x) (* x x)))))))

(test-ffi
 "f32 param and result"
 16.0
 (let ()
   (define-foreign fsquare
     "math" "fsquare"
     f32 -> f32)
   (fsquare 4.0))
 `(("math" . (("fsquare" . ,(lambda (x) (* x x)))))))

(test-ffi
 "f64 param and result"
 16.0
 (let ()
   (define-foreign fsquare
     "math" "fsquare"
     f64 -> f64)
   (fsquare 4.0))
 `(("math" . (("fsquare" . ,(lambda (x) (* x x)))))))

(test-ffi
 "string param and result"
 "Hello, owl!"
 (let ()
   (define-foreign hello
     "host" "hello"
     (ref string) -> (ref string))
   (hello "owl"))
 `(("host" .
    (("hello" . ,(lambda (name) (string-append "Hello, " name "!")))))))

(test-ffi
 "eq param and result"
 "hello"
 (let ()
   (define-foreign echo
     "host" "echo"
     (ref eq) -> (ref eq))
   (echo "hello"))
 `(("host" .
    (("echo" . ,(lambda (x) x))))))

(test-ffi
 "external?"
 #t
 (let ()
   (define-foreign get-extern
     "host" "getExtern"
     -> (ref null extern))
   (external? (get-extern)))
 `(("host" . (("getExtern" . ,(lambda () '(external value)))))))

(test-ffi
 "external-null?"
 #t
 (let ()
   (define-foreign get-null
     "host" "getNull"
     -> (ref null extern))
   (external-null? (get-null)))
 `(("host" . (("getNull" . ,(lambda () #f))))))

(test-ffi
 "external-non-null?"
 #t
 (let ()
   (define-foreign get-non-null
     "host" "getNonNull"
     -> (ref extern))
   (external-non-null? (get-non-null)))
 `(("host" . (("getNonNull" . ,(lambda () #t))))))

(test-ffi
 "procedure->extern"
 1
 (let ((counter 0))
   (define-foreign callback
     "host" "callback"
     (ref null extern) -> none)
   (callback
    (procedure->external
     (lambda () (set! counter (+ counter 1)))))
   counter)
 `(("host" .
    (("callback" . ,(lambda (f) (f) *unspecified*))))))

(test-end* "test-ffi")
