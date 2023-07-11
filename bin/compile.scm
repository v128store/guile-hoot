;;; Test script to compile a Scheme expression to wasm
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

(use-modules (wasm assemble)
             (hoot compile)
             (ice-9 binary-ports)
             (ice-9 match))

(define d8 (or (getenv "D8") "d8"))
(define srcdir (or (getenv "SRCDIR") (getcwd)))

(define (scope-file file-name)
  (in-vicinity srcdir file-name))

(define* (compile-expr expr out #:key import-abi? export-abi?)
  (let ((bytes (assemble-wasm
                (compile expr
                         #:import-abi? import-abi?
                         #:export-abi? export-abi?
                         #:dump-cps? #t
                         #:dump-wasm? #t))))
    (call-with-output-file out
      (lambda (port) (put-bytevector port bytes)))))

(define (read1 str)
  (call-with-input-string
   str
   (lambda (port)
     (let ((expr (read port)))
       (when (eof-object? expr)
         (error "No expression to evaluate"))
       (let ((tail (read port)))
         (unless (eof-object? tail)
           (error "Unexpected trailing expression" tail)))
       expr))))

(when (batch-mode?)
  (match (program-arguments)
    ((arg0 . args)
     (let lp ((args args) (import-abi? #f) (export-abi? #f))
       (match args
         (("--import-abi" . args) (lp args #t export-abi?))
         (("--export-abi" . args) (lp args import-abi? #t))
         ((str out) (compile-expr (read1 str) out
                                  #:import-abi? import-abi?
                                  #:export-abi? export-abi?))
         (_
          (format (current-error-port) "usage: ~a EXPR OUT.WASM\n" arg0)
          (exit 1)))))))
