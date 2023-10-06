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
            use-d8?
            use-hoot-vm?
            test-compilation
            test-call
            test-end*))

(define d8 (or (getenv "D8") "d8"))
(define srcdir (or (getenv "SRCDIR") (getcwd)))
(define test-hosts (string-split (or (getenv "WASM_HOST") "d8,hoot") #\,))
(define use-d8? (member "d8" test-hosts))
(define use-hoot-vm? (member "hoot" test-hosts))

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
  (let* ((args (cons* "--experimental-wasm-stringref" args))
         (port (apply open-pipe* OPEN_READ d8 args))
         (output (get-string-all port)))
    (close-port port)
    (string-trim-both output)))

(define (compile-value/d8 wasm)
  (call-with-compiled-wasm-file
   wasm
   (lambda (wasm-file-name)
     (run-d8 (scope-file "test/load-wasm-and-print.js") "--" srcdir wasm-file-name))))

(define (compile-call/d8 proc . args)
  (let lp ((modules (cons proc args)) (files '()) (first? #t))
    (match modules
      (()
       (apply run-d8 (scope-file "test/test-call.js") "--" srcdir (reverse files)))
      ((module . rest)
       (call-with-compiled-wasm-file
        module
        (lambda (file)
          (lp rest (cons file files) #f)))))))

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

(define (compile-value/hoot wasm)
  (call-with-printed-values
   (lambda ()
     (hoot-load (hoot-instantiate reflect-wasm wasm)))))

(define (compile-call/hoot proc . args)
  (call-with-printed-values
   (lambda ()
     (let* ((proc-module (hoot-instantiate reflect-wasm proc))
            (proc* (hoot-load proc-module))
            (reflector (hoot-module-reflector proc-module))
            (args* (map (lambda (arg)
                          (hoot-load
                           (hoot-instantiate reflector arg)))
                        args)))
       (apply proc* args*)))))

(define (compare-results hoot-result d8-result)
  (cond
   ((and use-hoot-vm? use-d8?)
    (unless (equal? hoot-result d8-result)
      (error "our result differs from d8" hoot-result d8-result))
    hoot-result)
   (use-d8? d8-result)
   (else hoot-result)))

(define (compile-value* expr)
  (let ((wasm (compile expr)))
    (compare-results (and use-hoot-vm? (compile-value/hoot wasm))
                     (and use-d8? (compile-value/d8 wasm)))))

(define (compile-call* proc . args)
  (let ((proc* (compile proc))
        (args* (map (lambda (exp)
                      (compile exp #:import-abi? #t #:export-abi? #f))
                    args)))
    (compare-results (and use-hoot-vm? (apply compile-call/hoot proc* args*))
                     (and use-d8? (apply compile-call/d8 proc* args*)))))

(define-syntax-rule (test-compilation expr repr)
  (test-equal repr repr (compile-value* 'expr)))

(define-syntax-rule (test-call repr proc arg ...)
  (test-equal repr repr (compile-call* 'proc 'arg ...)))

(define-syntax-rule (test-end* name)
  (begin
    (when (and (batch-mode?)
               (or (not (zero? (test-runner-fail-count (test-runner-get))))
                   (not (zero? (test-runner-xpass-count (test-runner-get))))))
      (force-output)
      (exit 1))
    (test-end name)))
