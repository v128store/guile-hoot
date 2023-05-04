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

(define-syntax-rule (test-compilation expr repr)
  (test-equal repr repr (compile-scheme-then-load-wasm-in-d8 'expr)))

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

(test-end "test-constants")

