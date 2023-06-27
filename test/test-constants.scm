;;; WebAssembly binary parser
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
;;; Parser for WebAssembly binary format
;;;
;;; Code:

(use-modules (wasm assemble)
             (hoot compile)
             (ice-9 binary-ports)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-64))

(define d8 (or (getenv "D8") "d8"))
(define srcdir (or (getenv "SRCDIR") (getcwd)))

(define (scope-file file-name)
  (string-append srcdir "/" file-name))

(test-begin "test-constants")

(define (unwind-protect body unwind)
  (call-with-values
      (lambda ()
        (with-exception-handler
         (lambda (exn)
           (unwind)
           (raise-exception exn))
         body))
    (lambda vals
      (unwind)
      (apply values vals))))

(define (call-with-compiled-wasm-file wasm f)
  (let* ((wasm-port (mkstemp "/tmp/tmp-wasm-XXXXXX"))
         (wasm-file-name (port-filename wasm-port)))
    (put-bytevector wasm-port (assemble-wasm wasm))
    (close-port wasm-port)
    (unwind-protect
     (lambda () (f wasm-file-name))
     (lambda () (delete-file wasm-file-name)))))

(define (run-d8 . args)
  (let* ((args (cons* "--experimental-wasm-gc"
                      "--experimental-wasm-stringref"
                      "--experimental-wasm-return-call"
                      args))
         (port (apply open-pipe* OPEN_READ d8 args))
         (output (get-string-all port)))
    (close-port port)
    (string-trim-both output)))

(define (compile-scheme-then-load-wasm-in-d8 constant)
  (call-with-compiled-wasm-file
   (compile constant)
   (lambda (wasm-file-name)
     (run-d8 (scope-file "test/load-wasm-and-print.js") "--" srcdir wasm-file-name))))

(define (compile-call form)
  (let lp ((form form) (files '()) (first? #t))
    (match form
      (()
       (apply run-d8 (scope-file "test/test-call.js") "--" srcdir (reverse files)))
      ((x . form)
       (call-with-compiled-wasm-file
        (compile x #:import-abi? (not first?) #:export-abi? first?)
        (lambda (file)
          (lp form (cons file files) #f)))))))

(define-syntax-rule (test-compilation expr repr)
  (test-equal repr repr (compile-scheme-then-load-wasm-in-d8 'expr)))
(define-syntax-rule (test-call repr proc arg ...)
  (test-equal repr repr (compile-call '(proc arg ...))))

(test-compilation 42 "42")
(test-compilation 100 "100")
(test-compilation -1 "-1")
(test-compilation #f "false")
(test-compilation '#nil "#nil")
(test-compilation '() "()")
(test-compilation #t "true")
(test-compilation #\a "#\\x61")
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
(test-compilation #*0100000 "#*0100000")
(test-compilation "foo" "foo")
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
(test-call "120" (lambda (n)
                   (let fac ((n n))
                     (if (eq? n 0)
                         1
                         (* n (fac (1- n))))))
           5)
(test-call "9227465" (lambda (n)
                       (let fib ((n n))
                         (if (<= n 1)
                             1
                             (+ (fib (- n 1)) (fib (- n 2))))))
           34)

(test-call "500000000" (lambda ()
                         (let lp ((n 0))
                           (if (< n #e5e8)
                               (lp (1+ n))
                               n))))

(test-call "8" (lambda (a b) (logand a b)) #b1100 #b1010)
(test-call "14" (lambda (a b) (logior a b)) #b1100 #b1010)
(test-call "6" (lambda (a b) (logxor a b)) #b1100 #b1010)
(test-call "4" (lambda (a b) (logand a (lognot b))) #b1100 #b1010)

(test-call "(1 . 2)" (lambda (a b) (cons a b)) 1 2)
(test-call "1" (lambda (a) (car a)) '(1 . 2))
(test-call "2" (lambda (a) (cdr a)) '(1 . 2))

(test-call "#(1 2 3)" (lambda (a b c) (vector a b c)) 1 2 3)
(test-call "3" (lambda (v) (vector-ref v 2)) #(1 2 3))
(test-call "2" (lambda (v idx) (vector-ref v idx)) #(1 2 3) 1)
(test-call "#(42 42 42)" (lambda (n) (make-vector n 42)) 3)

(test-call "42" (lambda (n t) (when t (set! n 42)) n) 10 #t)
(test-call "10" (lambda (n t) (when t (set! n 42)) n) 10 #f)

(test-call "10" (lambda (bv) ((@ (rnrs bytevectors) bytevector-length) bv))
           #vu8(0 1 2 3 4 5 6 7 8 9))


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

(when (and (batch-mode?) (not (test-passed?)))
  (exit 1))

(test-end "test-constants")
