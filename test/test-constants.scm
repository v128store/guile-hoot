;;; WebAssembly binary parser
;;; Copyright (C) 2023 Igalia, S.L.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

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
     (run-d8 "load-wasm-and-print.js" "--" wasm-file-name))))

(define (compile-call form)
  (let lp ((form form) (files '()) (first? #t))
    (match form
      (()
       (apply run-d8 "test-call.js" "--" (reverse files)))
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

;; This is how you would debug outside the test suite...
;; (call-with-compiled-wasm-file
;;  (compile '(lambda (f) (+ (f) 1)) #:import-abi? #f #:export-abi? #t
;;           #:dump-wasm? #t)
;;  (lambda (proc)
;;    (call-with-compiled-wasm-file
;;     (compile '(lambda () 42) #:import-abi? #t #:export-abi? #f
;;              #:dump-wasm? #t)
;;     (lambda (arg)
;;       (copy-file proc "/tmp/proc.wasm")
;;       (copy-file arg "/tmp/arg.wasm")
;;       (pk (run-d8 "test-call.js" "--" proc arg))))))

(when (and (batch-mode?) (not (test-passed?)))
  (exit 1))

(test-end "test-constants")

