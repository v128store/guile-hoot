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

;; Imitate d8's output for now.
(define (print-values . vals)
  (define (d8-format x)
    (match x
      (#t (display "true"))
      (#f (display "false"))
      ((and (? number?) (? inexact?))
       (write
        (if (= x (truncate x))
            (inexact->exact x)
            x)))
      ((? eof-object?) (display "#eof"))
      ((? hoot-complex?)
       (d8-format (hoot-complex-real x))
       (display "+")
       (d8-format (hoot-complex-imag x))
       (display "i"))
      ((? hoot-fraction?)
       (d8-format (hoot-fraction-num x))
       (display "/")
       (d8-format (hoot-fraction-denom x)))
      ((or (? hoot-pair?) (? mutable-hoot-pair?))
       (display "(")
       (d8-format (hoot-pair-car x))
       (let loop ((cdr (hoot-pair-cdr x)))
         (match cdr
           (() #t)
           ((or (? hoot-pair?) (? mutable-hoot-pair?))
            (display " ")
            (d8-format (hoot-pair-car cdr))
            (loop (hoot-pair-cdr cdr)))
           (x
            (display " . ")
            (d8-format x))))
       (display ")"))
      ((or (? hoot-vector?) (? mutable-hoot-vector?))
       (let ((k (hoot-vector-length x)))
         (display "#(")
         (unless (= k 0)
           (do ((i 0 (+ i 1)))
               ((= i (- k 1)))
             (d8-format (hoot-vector-ref x i))
             (display " "))
           (d8-format (hoot-vector-ref x (- k 1))))
         (display ")")))
      ((or (? hoot-bytevector?) (? mutable-hoot-bytevector?))
       (let ((k (hoot-bytevector-length x)))
         (display "#vu8(")
         (unless (= k 0)
           (do ((i 0 (+ i 1)))
               ((= i (- k 1)))
             (display (hoot-bytevector-ref x i))
             (display " "))
           (display (hoot-bytevector-ref x (- k 1))))
         (display ")")))
      ((or (? hoot-bitvector?)
           (? mutable-hoot-bitvector?))
       (let ((k (hoot-bitvector-length x)))
         (display "#*")
         (do ((i 0 (+ i 1)))
             ((= i k))
           (display (hoot-bitvector-ref x i)))))
      ((? string?)
       (display x))
      ((? mutable-hoot-string?)
       (display (mutable-hoot-string->string x)))
      ((? hoot-procedure?)
       (display "#<procedure>"))
      ((? hoot-symbol?)
       (display (hoot-symbol-name x)))
      ((? hoot-keyword?)
       (format #t "#:~a" (hoot-keyword-name x)))
      (_ (write x))))
  (string-trim-both
   (with-output-to-string
     (lambda ()
       (for-each (lambda (x)
                   (d8-format x)
                   (newline))
                 vals)))))

(define (compile-value/hoot expr)
  (call-with-values (lambda () (compile-value reflect-wasm expr))
    print-values))

(define (compile-call/hoot form)
  (call-with-values (lambda () (apply compile-call reflect-wasm form))
    print-values))

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
