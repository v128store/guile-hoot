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
;;; Constant value tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-constants")

(test-compilation 42 "42")
(test-compilation 100 "100")
(test-compilation -1 "-1")
(test-compilation -42 "-42")
(test-compilation 1.0 "1.0")
(test-compilation 1.5 "1.5")
(test-compilation 1/4 "1/4")
(test-compilation 4.0+2.0i "4.0+2.0i")
(test-compilation #f "#f")
(test-compilation '#nil "#nil")
(test-compilation '() "()")
(test-compilation #t "#t")
(test-compilation #\a "#\\a")
(test-compilation (if #f #f) "#<unspecified>")
;(test-compilation the-eof-object "#<eof>")
(test-compilation '(1 . 2) "(1 . 2)")
(test-compilation '(1 2 3 4) "(1 2 3 4)")
(test-compilation #() "#()")
(test-compilation #(3 10 (42)) "#(3 10 (42))")
(test-compilation #vu8() "#vu8()")
(test-compilation #vu8(3 10 42) "#vu8(3 10 42)")
(test-compilation #* "#*")
(test-compilation #*101 "#*101")
(test-compilation #*100000 "#*100000")
(test-compilation #*000000000000000000000000000000001 "#*000000000000000000000000000000001")
(test-compilation "foo" "\"foo\"")
(test-compilation (lambda () 42) "#<procedure>")
(test-compilation #:foo "#:foo")
(test-compilation 'TZAG "TZAG")

(test-call "42" (lambda () 42))
(test-call "69" (lambda (x) x) 69)
(test-call "hey" (lambda (x) (if x 'hey 'ho)) #t)
(test-call "hey2" (lambda (x) (if x 'hey2 'ho)) 42)
(test-call "ho" (lambda (x) (if x 'hey3 'ho)) #f)
(test-call "10" (lambda (x y) (+ x y)) 6 4)
(test-call "rerro" (lambda () 'rerro))
(test-call "1337" (lambda (f) (f)) (lambda () 1337))
(test-call "43" (lambda (f) (+ (f) 1)) (lambda () 42))

;; This is how you would debug outside the test suite...
;; (call-with-compiled-wasm-file
;;  (compile '(lambda (n)
;;              (let fib ((n n))
;;                          (if (<= n 1)
;;                              1
;;                              (+ (fib (- n 1)) (fib (- n 2))))))
;;           #:import-abi? #f #:export-abi? #t #:dump-cps? #t #:dump-wasm? #t)
;;  (lambda (proc)
;;    (call-with-compiled-wasm-file
;;     (compile 40 #:import-abi? #t #:export-abi? #f)
;;     (lambda (arg)
;;       (copy-file proc "/tmp/proc.wasm")
;;       (copy-file arg "/tmp/arg.wasm")
;;       (pk (run-d8 "test-call.js" "--" proc arg))))))

(test-end* "test-constants")
