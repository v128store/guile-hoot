;;; WebAssembly reflection
;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
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
;;; Reflection for Hoot-compiled WASM modules.
;;;
;;; Code:

(define-module (hoot reflect)
  #:use-module (hoot compile)
  #:use-module (ice-9 match)
  #:use-module (ice-9 binary-ports)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (wasm assemble)
  #:use-module (wasm parse)
  #:use-module (wasm vm)
  #:export (hoot-instantiate
            hoot-load
            hoot-call
            compile-call
            compile-value))

(define (s64? x)
  (and (exact-integer? x) (< (- (ash -1 63) 1) x (ash 1 63))))

(define (u64? x)
  (and (exact-integer? x) (< -1 x (- (ash 1 64) 1))))

(define-record-type <reflector>
  (make-reflector instance abi)
  reflector?
  (instance reflector-instance)
  (abi reflector-abi))

(define-record-type <hoot-module>
  (make-hoot-module reflector instance)
  hoot-module?
  (reflector hoot-module-reflector)
  (instance hoot-module-instance))

(define-record-type <hoot-complex>
  (make-hoot-complex reflector obj real imag)
  hoot-complex?
  (reflector hoot-complex-reflector)
  (obj hoot-complex-obj)
  (real hoot-complex-real)
  (imag hoot-complex-imag))

(define-record-type <hoot-fraction>
  (make-hoot-fraction reflector obj num denom)
  hoot-fraction?
  (reflector hoot-fraction-reflector)
  (obj hoot-fraction-obj)
  (num hoot-fraction-num)
  (denom hoot-fraction-denom))

(define-record-type <hoot-pair>
  (make-hoot-pair reflector obj car cdr)
  hoot-pair?
  (reflector hoot-pair-reflector)
  (obj hoot-pair-obj)
  (car hoot-pair-car)
  (cdr hoot-pair-cdr))

(define-record-type <mutable-hoot-pair>
  (make-mutable-hoot-pair reflector obj car cdr)
  mutable-hoot-pair?
  (reflector mutable-hoot-pair-reflector)
  (obj mutable-hoot-pair-obj)
  (car mutable-hoot-pair-car)
  (cdr mutable-hoot-pair-cdr))

(define-record-type <hoot-vector>
  (make-hoot-vector reflector obj)
  hoot-vector?
  (reflector hoot-vector-reflector)
  (obj hoot-vector-obj))

(define-record-type <mutable-hoot-vector>
  (make-mutable-hoot-vector reflector obj)
  mutable-hoot-vector?
  (reflector mutable-hoot-vector-reflector)
  (obj mutable-hoot-vector-obj))

(define-record-type <hoot-bytevector>
  (make-hoot-bytevector reflector obj)
  hoot-bytevector?
  (reflector hoot-bytevector-reflector)
  (obj hoot-bytevector-obj))

(define-record-type <mutable-hoot-bytevector>
  (make-mutable-hoot-bytevector reflector obj)
  mutable-hoot-bytevector?
  (reflector mutable-hoot-bytevector-reflector)
  (obj mutable-hoot-bytevector-obj))

(define-record-type <hoot-bitvector>
  (make-hoot-bitvector reflector obj)
  hoot-bitvector?
  (reflector hoot-bitvector-reflector)
  (obj hoot-bitvector-obj))

(define-record-type <mutable-hoot-bitvector>
  (make-mutable-hoot-bitvector reflector obj)
  mutable-hoot-bitvector?
  (reflector mutable-hoot-bitvector-reflector)
  (obj mutable-hoot-bitvector-obj))

(define-record-type <mutable-hoot-string>
  (make-mutable-hoot-string reflector obj)
  mutable-hoot-string?
  (reflector mutable-hoot-string-reflector)
  (obj mutable-hoot-string-obj))

(define-record-type <hoot-procedure>
  (make-hoot-procedure reflector obj)
  hoot-procedure?
  (reflector hoot-procedure-reflector)
  (obj hoot-procedure-obj))

(define-record-type <hoot-symbol>
  (make-hoot-symbol reflector obj)
  hoot-symbol?
  (reflector hoot-symbol-reflector)
  (obj hoot-symbol-obj))

(define-record-type <hoot-keyword>
  (make-hoot-keyword reflector obj)
  hoot-keyword?
  (reflector hoot-keyword-reflector)
  (obj hoot-keyword-obj))

(define-syntax-rule (~ reflector name args ...)
  ((wasm-instance-export-ref (reflector-instance reflector) name) args ...))

;; UH OH: This doesn't detect cycles!
(define (%hoot-print obj port)
  (match obj
    ((or #t #f () #nil (? number?) (? eof-object?)
         (? unspecified?) (? char?) (? string?))
     (write obj port))
    (($ <hoot-complex> _ _ real imag)
     (%hoot-print real port)
     (display "+" port)
     (%hoot-print imag port)
     (display "i" port))
    (($ <hoot-fraction> _ _ num denom)
     (%hoot-print num port)
     (display "/" port)
     (%hoot-print denom port))
    ((or ($ <hoot-pair> _ _ car cdr)
         ($ <mutable-hoot-pair> _ _ car cdr))
     (display "(" port)
     (%hoot-print car port)
     (let loop ((cdr cdr))
       (match cdr
         (() #t)
         ((or ($ <hoot-pair> _ _ car cdr)
              ($ <mutable-hoot-pair> _ _ car cdr))
          (display " " port)
          (%hoot-print car port)
          (loop cdr))
         (obj
          (display " . " port)
          (%hoot-print obj port))))
     (display ")" port))
    ((or ($ <hoot-vector> reflector v)
         ($ <mutable-hoot-vector> reflector v))
     (let ((k (~ reflector "vector_length" v)))
       (display "#(" port)
       (do ((i 0 (+ i 1)))
           ((= i (- k 1)))
         (%hoot-print (~ reflector "vector_ref" v i) port)
         (display " " port))
       (%hoot-print (~ reflector "vector_ref" v (- k 1)) port)
       (display ")" port)))
    ((or ($ <hoot-bytevector> reflector bv)
         ($ <mutable-hoot-bytevector> reflector bv))
     (let ((k (~ reflector "bytevector_length" bv)))
       (display "#vu8(" port)
       (do ((i 0 (+ i 1)))
           ((= i (- k 1)))
         (display (~ reflector "bytevector_ref" bv i) port)
         (display " " port))
       (display (~ reflector "bytevector_ref" bv (- k 1)) port)
       (display ")" port)))
    ((or ($ <hoot-bitvector> reflector bv)
         ($ <mutable-hoot-bitvector> reflector bv))
     (let ((k (~ reflector "bitvector_length" bv)))
       (display "#*" port)
       (do ((i 0 (+ i 1)))
           ((= i k))
         (display (~ reflector "bitvector_ref" bv i) port))))
    (($ <mutable-hoot-string> reflector str)
     (display (~ reflector "string_value" str) port))
    (($ <hoot-procedure>)
     (display "procedure" port))
    (($ <hoot-symbol> reflector sym)
     (display (~ reflector "symbol_name" sym) port))
    (($ <hoot-keyword> reflector sym)
     (display (~ reflector "keyword_name" sym) port))))

(define (hoot-print obj port)
  (display "#<hoot " port)
  (%hoot-print obj port)
  (display ">" port))

(for-each (lambda (rtd) (set-record-type-printer! rtd hoot-print))
          (list <hoot-complex>
                <hoot-fraction>
                <hoot-pair>
                <mutable-hoot-pair>
                <hoot-vector>
                <mutable-hoot-vector>
                <hoot-bytevector>
                <mutable-hoot-bytevector>
                <hoot-bitvector>
                <mutable-hoot-bitvector>
                <mutable-hoot-string>
                <hoot-procedure>
                <hoot-symbol>
                <hoot-keyword>))

(define (wasm->guile reflector x)
  (match (~ reflector "describe" x)
    ("fixnum" (~ reflector "fixnum_value" x))
    ("char" (integer->char (~ reflector "char_value" x)))
    ("string" (~ reflector "string_value" x))
    ("mutable-string" (make-mutable-hoot-string reflector x))
    ("true" #t)
    ("false" #f)
    ("eof" (eof-object))
    ("nil" #nil)
    ("null" '())
    ("unspecified" *unspecified*)
    ("flonum" (~ reflector "flonum_value" x))
    ("bignum" (~ reflector "bignum_value" x))
    ("complex"
     (make-hoot-complex reflector x
                        (~ reflector "complex_real" x)
                        (~ reflector "complex_imag" x)))
    ("fraction"
     (make-hoot-fraction reflector x
                         (~ reflector "fraction_num" x)
                         (~ reflector "fraction_denom" x)))
    ("symbol" (make-hoot-symbol reflector x))
    ("keyword" (make-hoot-keyword reflector x))
    ("pair"
     (make-hoot-pair reflector x
                     (wasm->guile reflector (~ reflector "car" x))
                     (wasm->guile reflector (~ reflector "cdr" x))))
    ("mutable-pair"
     (make-mutable-hoot-pair reflector x
                             (wasm->guile reflector (~ reflector "car" x))
                             (wasm->guile reflector (~ reflector "cdr" x))))
    ("vector" (make-hoot-vector reflector x))
    ("mutable-vector" (make-mutable-hoot-vector reflector x))
    ("bytevector" (make-hoot-bytevector reflector x))
    ("mutable-bytevector" (make-mutable-hoot-bytevector reflector x))
    ("bitvector" (make-hoot-bitvector reflector x))
    ("mutable-bitvector" (make-mutable-hoot-bitvector reflector x))
    ("procedure" (make-hoot-procedure reflector x))))

(define (guile->wasm reflector x)
  (match x
    ((and (? number?) (? inexact?)) (~ reflector "scm_from_f64" x))
    ((? exact-integer?)
     (if (<= (~ reflector "scm_most_negative_fixnum")
             x
             (~ reflector "scm_most_positive_fixnum"))
         (~ reflector "scm_from_fixnum" x)
         (~ reflector "scm_from_bignum" x)))
    ((and (? number?) (? exact?)) (~ reflector "scm_from_fraction" (numerator x) (denominator x)))
    ((? complex?) (~ reflector "scm_from_complex" (real-part x) (imag-part x)))
    (#t (~ reflector "scm_true"))
    (#f (~ reflector "scm_false"))
    (#nil (~ reflector "scm_nil"))
    (() (~ reflector "scm_null"))
    ((? unspecified?) (~ reflector "scm_unspecified"))
    ((? eof-object?) (~ reflector "scm_eof"))
    ((? char?) (~ reflector "scm_from_char" (char->integer x)))
    ((? string?) (~ reflector "scm_from_string" x))
    ((or ($ <hoot-complex> _ obj)
         ($ <hoot-fraction> _ obj)
         ($ <hoot-pair> _ obj)
         ($ <mutable-hoot-pair> _ obj)
         ($ <hoot-vector> _ obj)
         ($ <mutable-hoot-vector> _ obj)
         ($ <hoot-bytevector> _ obj)
         ($ <mutable-hoot-bytevector> _ obj)
         ($ <hoot-bitvector> _ obj)
         ($ <mutable-hoot-bitvector> _ obj)
         ($ <mutable-hoot-string> _ obj)
         ($ <hoot-procedure> _ obj)
         ($ <hoot-symbol> _ obj)
         ($ <hoot-keyword> _ obj))
     obj)))

(define %runtime-imports
  `(("rt" .
     (("bignum_from_i32" . ,identity)
      ("bignum_from_i64" . ,identity)
      ("bignum_from_u64" . ,identity)
      ("bignum_to_f64" . ,exact->inexact)
      ("bignum_is_i64" . ,s64?)
      ("bignum_is_u64" . ,u64?)
      ("bignum_get_i64" . ,identity)
      ("bignum_add" . ,+)
      ("bignum_sub" . ,-)
      ("bignum_mul" . ,*)
      ("bignum_quo" . ,quotient)
      ("bignum_rem" . ,remainder)
      ("bignum_mod" . ,modulo)
      ("bignum_gcd" . ,gcd)
      ("make_weak_map" . ,make-weak-key-hash-table)
      ("weak_map_get" . ,hash-ref)
      ("weak_map_set" . ,hash-set!)
      ("weak_map_delete" . ,hash-remove!)
      ("fsin" . ,sin)
      ("fcos" . ,cos)
      ("ftan" . ,tan)
      ("fasin" . ,asin)
      ("facos" . ,acos)
      ("fatan" . ,atan)
      ("fatan2" . ,atan)
      ("die" . ,(lambda (key . args)
                  (apply throw (string->symbol key) args)))))))

(define (make-abi-imports instance)
  `(("abi" . ,(map (lambda (name)
                     (cons name (wasm-instance-export-ref instance name)))
                   (wasm-instance-export-names instance)))))

(define (hoot-instantiate reflector scheme-wasm)
  (define (instantiate wasm imports)
    (make-wasm-instance (make-wasm-module wasm) #:imports imports))
  ;; You can either pass an existing reflector and import its ABI, or
  ;; pass a parsed reflection WASM module and create a new reflector.
  (if (reflector? reflector)
      (let* ((imports (append %runtime-imports (reflector-abi reflector)))
             (instance (instantiate scheme-wasm imports)))
        (make-hoot-module reflector instance))
      (let* ((instance (instantiate scheme-wasm %runtime-imports))
             (abi (make-abi-imports instance))
             (reflector (make-reflector (instantiate reflector abi) abi)))
        (make-hoot-module reflector instance))))

(define (hoot-call proc . args)
  (match proc
    (($ <hoot-procedure> reflector f)
     (let ((argv (~ reflector "make_vector"
                    (+ (length args) 1)
                    (~ reflector "scm_false"))))
       (~ reflector "vector_set" argv 0 f)
       (let loop ((args args) (i 1))
         (match args
           (() #t)
           ((arg . rest)
            (~ reflector "vector_set" argv i (guile->wasm reflector arg))
            (loop rest (+ i 1)))))
       (let* ((results (~ reflector "call" f argv))
              (n-results (~ reflector "vector_length" results)))
         (apply values
                (let loop ((i 0))
                  (if (= i n-results)
                      '()
                      (let ((result (~ reflector "vector_ref" results i)))
                        (cons (wasm->guile reflector result)
                              (loop (+ i 1))))))))))))

(define (hoot-load module)
  (match module
    (($ <hoot-module> reflector instance)
     (let* (($load (wasm-instance-export-ref instance "$load")))
       (hoot-call (wasm->guile reflector (wasm-global-ref $load)))))))

;; Assembling and then parsing the WASM because resolve-wasm isn't
;; yet equivalent to the parsed form.
(define (compile* exp import-abi? export-abi?)
  (call-with-input-bytevector (assemble-wasm
                               (compile exp
                                        #:import-abi? import-abi?
                                        #:export-abi? export-abi?))
                              parse-wasm))

(define (compile-value reflect-wasm exp)
  (hoot-load (hoot-instantiate reflect-wasm (compile* exp #f #t))))

(define (compile-call reflect-wasm proc-exp . arg-exps)
  (let* ((proc-module (hoot-instantiate reflect-wasm (compile* proc-exp #f #t)))
         (proc (hoot-load proc-module))
         (reflector (hoot-module-reflector proc-module))
         (args (map (lambda (exp)
                      (hoot-load
                       (hoot-instantiate reflector (compile* exp #t #f))))
                    arg-exps)))
    (apply hoot-call proc args)))
