;;; Test script to show high-level optimization of Scheme expressions
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

(use-modules (language tree-il)
             (system base compile)
             (system base language)
             (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 textual-ports))

(define default-hoot-environment
  (@@ (hoot compile) default-hoot-environment))
(define wrap-with-prelude
  (@@ (hoot compile) wrap-with-prelude))

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

(define* (optimize expr #:key (optimization-level (default-optimization-level)) (opts '()))
  (define lower-tree-il
    ((language-lowerer (lookup-language 'tree-il)) optimization-level opts))

  (let* ((expr (wrap-with-prelude expr #:import-abi? #t))
         (env (default-hoot-environment))
         (tree-il (compile expr #:from 'scheme #:to 'tree-il
                           #:env env
                           #:optimization-level 2
                           #:warning-level 2))
         (optimized (lower-tree-il tree-il env)))
    (pretty-print (tree-il->scheme optimized env))))

(when (batch-mode?)
  (match (program-arguments)
    ((arg0 expr)
     (optimize (read1 expr)))
    ((arg0 . _)
     (format (current-error-port) "usage: ~a EXPR\n" arg0)
     (exit 1))))
