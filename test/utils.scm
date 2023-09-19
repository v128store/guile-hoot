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
;;; Test utilities.
;;;
;;; Code:

(define-module (test utils)
  #:use-module (wasm assemble)
  #:use-module (wasm parse)
  #:use-module (hoot compile)
  #:use-module (hoot reflect)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-64)
  #:export (d8
            srcdir
            use-hoot-vm?
            test-compilation
            test-call
            test-end*))

(define d8 (or (getenv "D8") "d8"))
(define srcdir (or (getenv "SRCDIR") (getcwd)))
(define use-hoot-vm? (equal? (getenv "USE_HOOT_VM") "1"))

(define (scope-file file-name)
  (string-append srcdir "/" file-name))

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

(define (compile-value/d8 constant)
  (call-with-compiled-wasm-file
   (compile constant)
   (lambda (wasm-file-name)
     (run-d8 (scope-file "test/load-wasm-and-print.js") "--" srcdir wasm-file-name))))

(define (compile-call/d8 form)
  (let lp ((form form) (files '()) (first? #t))
    (match form
      (()
       (apply run-d8 (scope-file "test/test-call.js") "--" srcdir (reverse files)))
      ((x . form)
       (call-with-compiled-wasm-file
        (compile x #:import-abi? (not first?) #:export-abi? first?)
        (lambda (file)
          (lp form (cons file files) #f)))))))

(define reflect-wasm
  (call-with-input-file "js-runtime/reflect.wasm" parse-wasm))

(define (call-with-printed-values thunk)
  (string-trim-both
   (with-output-to-string
     (lambda ()
       (call-with-values thunk
         (lambda vals
           (for-each (lambda (x)
                       ((@@ (hoot reflect) %hoot-print) x (current-output-port))
                       (newline))
                     vals)))))))

(define (compile-value/hoot expr)
  (call-with-printed-values
   (lambda ()
     (compile-value reflect-wasm expr))))

(define (compile-call/hoot form)
  (call-with-printed-values
   (lambda ()
     (apply compile-call reflect-wasm form))))

(define (compile-value* expr)
  (if use-hoot-vm?
      (compile-value/hoot expr)
      (compile-value/d8 expr)))

(define (compile-call* form)
  (if use-hoot-vm?
      (compile-call/hoot form)
      (compile-call/d8 form)))

(define-syntax-rule (test-compilation expr repr)
  (test-equal repr repr (compile-value* 'expr)))

(define-syntax-rule (test-call repr proc arg ...)
  (test-equal repr repr (compile-call* '(proc arg ...))))

(define-syntax-rule (test-end* name)
  (begin
    (when (and (batch-mode?)
               (or (not (zero? (test-runner-fail-count (test-runner-get))))
                   (not (zero? (test-runner-xpass-count (test-runner-get))))))
      (force-output)
      (exit 1))
    (test-end name)))
