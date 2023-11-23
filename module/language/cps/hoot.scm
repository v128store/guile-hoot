;;; Continuation-passing style (CPS) intermediate language (IL)

;; Copyright (C) 2023 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU Lesser General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Backend-specific lowering and optimization when targetting the Hoot
;;; Wasm/GC run-time.
;;;
;;; Code:

(define-module (language cps hoot)
  #:use-module (ice-9 match)
  #:use-module (language cps dce)
  #:use-module (language cps simplify)
  #:use-module ((language cps utils) #:select (primcall-raw-representations))
  #:use-module (language cps verify)
  #:use-module (language cps hoot lower-primcalls)
  #:use-module (language cps hoot tailify)
  #:use-module (language cps hoot unify-returns)
  #:use-module (wasm types)
  #:export (hoot-primcall-raw-representations
            make-lowerer
            available-optimizations
            target-hash
            target-symbol-hash
            target-symbol-hash-bits
            target-keyword-hash
            target-has-unbound-boxes?))

(define (hoot-primcall-raw-representations name param)
  (case name
    ((restore) param) ;; param is list of representations.
    ((flonum->f64 compnum-real compnum-imag) '(f64))
    ((inline-wasm)
     (match param
       (($ <func> id
           ($ <type-use> #f ($ <func-sig> params results))
           locals body)
        (map (match-lambda
               (($ <ref-type> #f 'eq) 'scm)
               ('i64 's64)
               ('f64 'f64))
             results))))
    ((import-wasm) '())
    (else (primcall-raw-representations name param))))

(define *debug?* #f)

(define (maybe-verify program)
  (if *debug?*
      (verify program)
      program))

(define-syntax-rule (define-optimizer optimize (pass kw) ...)
  (define* (optimize program #:optional (opts '()))
    (let* ((program (maybe-verify program))
           (program (if (assq-ref opts kw)
                        (maybe-verify (pass program))
                        program))
           ...)
      program)))

(define (available-optimizations)
  '((#:eliminate-dead-code? 2)
    (#:simplify? 1)))

(define-optimizer optimize-hoot-backend-cps
  (eliminate-dead-code #:eliminate-dead-code?)
  (simplify #:simplify?))

(define (select-optimizations optimization-level opts all-opts)
  (define (kw-arg-ref args kw default)
    (match (memq kw args)
      ((_ val . _) val)
      (_ default)))
  (define (enabled-for-level? level) (<= level optimization-level))
  (let lp ((all-opts all-opts))
    (match all-opts
      (() '())
      (((kw level) . all-opts)
       (acons kw (kw-arg-ref opts kw (enabled-for-level? level))
              (lp all-opts))))))

(define (make-lowerer optimization-level opts)
  (let ((opts (select-optimizations optimization-level opts
                                    (available-optimizations))))
    (lambda (exp env)
      (optimize-hoot-backend-cps
       (unify-returns
        (tailify
         (lower-primcalls exp)
         #:primcall-raw-representations hoot-primcall-raw-representations))
       opts))))

;; Thomas Wang's 32-bit integer hasher, from
;; http://www.cris.com/~Ttwang/tech/inthash.htm.
(define (hash-i32 i)
  ;; 32-bit hash
  (define (i32 i) (logand i #xffffffff))
  (let* ((i (i32 i))
         (i (i32 (logxor (logxor i 61) (ash i -16))))
         (i (i32 (+ i (i32 (ash i 3)))))
         (i (i32 (logxor i (ash i -4))))
         (i (i32 (* i #x27d4eb2d))))
    (i32 (logxor i (ash i -15)))))
(define (finish-heap-object-hash h)
  (let ((h (hash-i32 h)))
    (if (= h 0)
        (hash-i32 42)
        h)))

;; FIXME: leakage of host hash function to guest.  Though it's valid for
;; our use case to generate hashq values, it's not reproducible.
(define (hashq-constant x)
  (finish-heap-object-hash (hash x (ash 1 32))))

(define (target-hash obj)
  (hashq-constant obj))

(define (target-symbol-hash str)
  (finish-heap-object-hash
   (string-fold (lambda (ch h)
                  (logand #xffffffff (+ (* h 31) (char->integer ch))))
                0
                str)))
(define target-symbol-hash-bits 32)

(define (target-keyword-hash str)
  (finish-heap-object-hash (target-symbol-hash str)))

(define target-has-unbound-boxes? #f)
