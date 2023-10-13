;;; WebAssembly reflection
;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
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
;;; Reflection for Hoot-compiled WASM modules.
;;;
;;; Code:

(define-module (hoot reflect)
  #:use-module (hoot compile)
  #:use-module (hoot config)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (wasm canonical-types)
  #:use-module (wasm parse)
  #:use-module (wasm types)
  #:use-module (wasm vm)
  #:export (hoot-object?
            hoot-complex?
            hoot-complex-real
            hoot-complex-imag
            hoot-fraction?
            hoot-fraction-num
            hoot-fraction-denom
            hoot-pair?
            mutable-hoot-pair?
            hoot-pair-car
            hoot-pair-cdr
            hoot-vector?
            mutable-hoot-vector?
            hoot-vector-length
            hoot-vector-ref
            hoot-bytevector?
            mutable-hoot-bytevector?
            hoot-bytevector-length
            hoot-bytevector-ref
            hoot-bitvector?
            mutable-hoot-bitvector?
            hoot-bitvector-length
            hoot-bitvector-ref
            hoot-symbol?
            hoot-symbol-name
            hoot-keyword?
            hoot-keyword-name
            mutable-hoot-string?
            mutable-hoot-string->string
            hoot-procedure?
            hoot-variable?
            hoot-atomic-box?
            hoot-hash-table?
            hoot-weak-table?
            hoot-fluid?
            hoot-dynamic-state?
            hoot-syntax?
            hoot-port?
            hoot-struct?

            hoot-module?
            hoot-module-reflector
            hoot-module-instance

            reflector?
            reflector-instance
            reflector-abi

            hoot-instantiate
            hoot-load
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

(set-record-type-printer! <reflector>
                          (lambda (r port)
                            (format port "#<reflector instance: ~a>"
                                    (reflector-instance r))))

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
  (car %hoot-pair-car)
  (cdr %hoot-pair-cdr))

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

(define-record-type <hoot-variable>
  (make-hoot-variable reflector obj)
  hoot-variable?
  (reflector hoot-variable-reflector)
  (obj hoot-variable-obj))

(define-record-type <hoot-atomic-box>
  (make-hoot-atomic-box reflector obj)
  hoot-atomic-box?
  (reflector hoot-atomic-box-reflector)
  (obj hoot-atomic-box-obj))

(define-record-type <hoot-hash-table>
  (make-hoot-hash-table reflector obj)
  hoot-hash-table?
  (reflector hoot-hash-table-reflector)
  (obj hoot-hash-table-obj))

(define-record-type <hoot-weak-table>
  (make-hoot-weak-table reflector obj)
  hoot-weak-table?
  (reflector hoot-weak-table-reflector)
  (obj hoot-weak-table-obj))

(define-record-type <hoot-fluid>
  (make-hoot-fluid reflector obj)
  hoot-fluid?
  (reflector hoot-fluid-reflector)
  (obj hoot-fluid-obj))

(define-record-type <hoot-dynamic-state>
  (make-hoot-dynamic-state reflector obj)
  hoot-dynamic-state?
  (reflector hoot-dynamic-state-reflector)
  (obj hoot-dynamic-state-obj))

(define-record-type <hoot-syntax>
  (make-hoot-syntax reflector obj)
  hoot-syntax?
  (reflector hoot-syntax-reflector)
  (obj hoot-syntax-obj))

(define-record-type <hoot-port>
  (make-hoot-port reflector obj)
  hoot-port?
  (reflector hoot-port-reflector)
  (obj hoot-port-obj))

(define-record-type <hoot-struct>
  (make-hoot-struct reflector obj)
  hoot-struct?
  (reflector hoot-struct-reflector)
  (obj hoot-struct-obj))

;; The Hoot procedure type is defined using Guile's low-level struct
;; API so that we can use applicable structs, allowing Hoot procedures
;; to be called as if they were native ones.
(define <hoot-procedure>
  (make-struct/no-tail <applicable-struct-vtable> 'pwpwpw))

(define (hoot-procedure? obj)
  (and (struct? obj) (eq? (struct-vtable obj) <hoot-procedure>)))

(define (make-hoot-procedure reflector obj)
  (define (hoot-apply . args)
    (hoot-call reflector obj args))
  (make-struct/no-tail <hoot-procedure> hoot-apply reflector obj))

(define (hoot-object? obj)
  (or (hoot-complex? obj)
      (hoot-fraction? obj)
      (hoot-pair? obj)
      (mutable-hoot-pair? obj)
      (hoot-vector? obj)
      (mutable-hoot-vector? obj)
      (hoot-bytevector? obj)
      (mutable-hoot-bytevector? obj)
      (hoot-bitvector? obj)
      (mutable-hoot-bitvector? obj)
      (mutable-hoot-string? obj)
      (hoot-procedure? obj)
      (hoot-symbol? obj)
      (hoot-keyword? obj)
      (hoot-variable? obj)
      (hoot-atomic-box? obj)
      (hoot-hash-table? obj)
      (hoot-weak-table? obj)
      (hoot-fluid? obj)
      (hoot-dynamic-state? obj)
      (hoot-syntax? obj)
      (hoot-port? obj)
      (hoot-struct? obj)))

(define-syntax-rule (~ reflector name args ...)
  ((wasm-instance-export-ref (reflector-instance reflector) name) args ...))

(define (hoot-pair-car pair)
  (match pair
    ((or ($ <hoot-pair> _ _ car) ($ <mutable-hoot-pair> _ _ car)) car)))

(define (hoot-pair-cdr pair)
  (match pair
    ((or ($ <hoot-pair> _ _ _ cdr) ($ <mutable-hoot-pair> _ _ _ cdr)) cdr)))

(define (hoot-vector-length vec)
  (match vec
    ((or ($ <hoot-vector> reflector obj)
         ($ <mutable-hoot-vector> reflector obj))
     (~ reflector "vector_length" obj))))

(define (hoot-vector-ref vec idx)
  (match vec
    ((or ($ <hoot-vector> reflector obj)
         ($ <mutable-hoot-vector> reflector obj))
     (wasm->guile reflector (~ reflector "vector_ref" obj idx)))))

(define (hoot-bytevector-length bv)
  (match bv
    ((or ($ <hoot-bytevector> reflector obj)
         ($ <mutable-hoot-bytevector> reflector obj))
     (~ reflector "bytevector_length" obj))))

(define (hoot-bytevector-ref bv idx)
  (match bv
    ((or ($ <hoot-bytevector> reflector obj)
         ($ <mutable-hoot-bytevector> reflector obj))
     (~ reflector "bytevector_ref" obj idx))))

(define (hoot-bitvector-length bv)
  (match bv
    ((or ($ <hoot-bitvector> reflector obj)
         ($ <mutable-hoot-bitvector> reflector obj))
     (~ reflector "bitvector_length" obj))))

(define (hoot-bitvector-ref bv idx)
  (match bv
    ((or ($ <hoot-bitvector> reflector obj)
         ($ <mutable-hoot-bitvector> reflector obj))
     (~ reflector "bitvector_ref" obj idx))))

(define (hoot-symbol-name sym)
  (match sym
    (($ <hoot-symbol> reflector obj)
     (~ reflector "symbol_name" obj))))

(define (hoot-keyword-name kw)
  (match kw
    (($ <hoot-keyword> reflector obj)
     (~ reflector "keyword_name" obj))))

(define (mutable-hoot-string->string str)
  (match str
    (($ <mutable-hoot-string> reflector obj)
     (~ reflector "string_value" obj))))

;; UH OH: This doesn't detect cycles!
(define (%hoot-print obj port)
  (match obj
    ((or #t #f () #nil (? number?) (? eof-object?)
         (? unspecified?) (? char?) (? string?))
     (write obj port))
    ((? hoot-complex?)
     (let ((real (hoot-complex-real obj))
           (imag (hoot-complex-imag obj)))
       (%hoot-print real port)
       (when (and (>= imag 0.0) (not (nan? imag)) (not (inf? imag)))
         (display "+" port))
       (%hoot-print imag port)
       (display "i" port)))
    ((? hoot-fraction?)
     (%hoot-print (hoot-fraction-num obj) port)
     (display "/" port)
     (%hoot-print (hoot-fraction-denom obj) port))
    ((or (? hoot-pair?) (? mutable-hoot-pair?))
     (display "(" port)
     (%hoot-print (hoot-pair-car obj) port)
     (let loop ((cdr (hoot-pair-cdr obj)))
       (match cdr
         (() #t)
         ((or (? hoot-pair?) (? mutable-hoot-pair?))
          (display " " port)
          (%hoot-print (hoot-pair-car cdr) port)
          (loop (hoot-pair-cdr cdr)))
         (obj
          (display " . " port)
          (%hoot-print obj port))))
     (display ")" port))
    ((or (? hoot-vector?) (? mutable-hoot-vector?))
     (let ((k (hoot-vector-length obj)))
       (display "#(" port)
       (unless (= k 0)
         (do ((i 0 (+ i 1)))
             ((= i (- k 1)))
           (%hoot-print (hoot-vector-ref obj i) port)
           (display " " port))
         (%hoot-print (hoot-vector-ref obj (- k 1)) port))
       (display ")" port)))
    ((or (? hoot-bytevector?) (? mutable-hoot-bytevector?))
     (let ((k (hoot-bytevector-length obj)))
       (display "#vu8(" port)
       (unless (= k 0)
         (do ((i 0 (+ i 1)))
             ((= i (- k 1)))
           (display (hoot-bytevector-ref obj i) port)
           (display " " port))
         (display (hoot-bytevector-ref obj (- k 1)) port))
       (display ")" port)))
    ((or (? hoot-bitvector?)
         (? mutable-hoot-bitvector?))
     (let ((k (hoot-bitvector-length obj)))
       (display "#*" port)
       (do ((i 0 (+ i 1)))
           ((= i k))
         (display (hoot-bitvector-ref obj i) port))))
    ((? mutable-hoot-string?)
     (write (mutable-hoot-string->string obj) port))
    ((? hoot-symbol?)
     (display (hoot-symbol-name obj) port))
    ((? hoot-keyword?)
     (format port "#:~a" (hoot-keyword-name obj)))
    ((? hoot-procedure?) (display "#<procedure>" port))
    ((? hoot-variable?) (display "#<variable>" port))
    ((? hoot-atomic-box?) (display "#<atomic-box>" port))
    ((? hoot-hash-table?) (display "#<hash-table>" port))
    ((? hoot-weak-table?) (display "#<weak-table>" port))
    ((? hoot-fluid?) (display "#<fluid>" port))
    ((? hoot-dynamic-state?) (display "#<dynamic-state>" port))
    ((? hoot-syntax?) (display "#<syntax>" port))
    ((? hoot-port?) (display "#<port>" port))
    ((? hoot-struct?) (display "#<struct>" port))))

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
                <hoot-keyword>
                <hoot-variable>
                <hoot-atomic-box>
                <hoot-hash-table>
                <hoot-weak-table>
                <hoot-fluid>
                <hoot-dynamic-state>
                <hoot-syntax>
                <hoot-port>
                <hoot-struct>))

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
                         (wasm->guile reflector (~ reflector "fraction_num" x))
                         (wasm->guile reflector (~ reflector "fraction_denom" x))))
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
    ("procedure" (make-hoot-procedure reflector x))
    ("variable" (make-hoot-variable reflector x))
    ("atomic-box" (make-hoot-atomic-box reflector x))
    ("hash-table" (make-hoot-hash-table reflector x))
    ("weak-table" (make-hoot-weak-table reflector x))
    ("fluid" (make-hoot-fluid reflector x))
    ("dynamic-state" (make-hoot-dynamic-state reflector x))
    ("syntax" (make-hoot-syntax reflector x))
    ("port" (make-hoot-port reflector x))
    ("struct" (make-hoot-struct reflector x))
    ("extern-ref" (~ reflector "extern_value" x))))

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
         ($ <hoot-procedure> _ _ obj)
         ($ <hoot-symbol> _ obj)
         ($ <hoot-keyword> _ obj)
         ($ <hoot-variable> _ obj)
         ($ <hoot-atomic-box> _ obj)
         ($ <hoot-hash-table> _ obj)
         ($ <hoot-weak-table> _ obj)
         ($ <hoot-fluid> _ obj)
         ($ <hoot-dynamic-state> _ obj)
         ($ <hoot-syntax> _ obj)
         ($ <hoot-port> _ obj)
         ($ <hoot-struct> _ obj))
     obj)
    (_ (~ reflector "scm_from_extern" x))))

(define wasm-array-vector (@@ (wasm vm) wasm-array-vector))
(define make-wasm-array (@@ (wasm vm) make-wasm-array))
(define wasm-array-set! (@@ (wasm vm) wasm-array-set!))
(define $wtf8 (canonicalize-type! (make-array-type #t 'i8)))

(define (wtf8->string wtf8)
  (let* ((vec (wasm-array-vector wtf8))
         (bv (make-bytevector (vector-length vec))))
    (do ((i 0 (+ i 1)))
        ((= i (vector-length vec)))
      (bytevector-u8-set! bv i (vector-ref vec i)))
    (utf8->string bv)))

(define (string->wtf8 str)
  (let* ((bv (string->utf8 str))
         (array (make-wasm-array $wtf8 (bytevector-length bv) 0)))
    (do ((i 0 (+ i 1)))
        ((= i (bytevector-length bv)))
      (wasm-array-set! array i (bytevector-u8-ref bv i)))
    array))

(define (logsub a b)
  (logand a (lognot b)))

(define (rsh a b)
  (ash a (- b)))

(define %runtime-imports
  `(("rt" .
     (("bignum_from_string" . ,string->number)
      ("bignum_from_i32" . ,identity)
      ("bignum_from_i64" . ,identity)
      ("bignum_from_u64" . ,identity)
      ("bignum_to_f64" . ,exact->inexact)
      ("bignum_is_i64" . ,s64?)
      ("bignum_is_u64" . ,u64?)
      ("bignum_get_i64" . ,identity)
      ("bignum_add" . ,+)
      ("bignum_sub" . ,-)
      ("bignum_mul" . ,*)
      ("bignum_lsh" . ,ash)
      ("bignum_rsh" . ,rsh)
      ("bignum_quo" . ,quotient)
      ("bignum_rem" . ,remainder)
      ("bignum_mod" . ,modulo)
      ("bignum_gcd" . ,gcd)
      ("bignum_logand" . ,logand)
      ("bignum_logior" . ,logior)
      ("bignum_logxor" . ,logxor)
      ("bignum_logsub" . ,logsub)
      ("bignum_lt" . ,<)
      ("bignum_le" . ,<=)
      ("bignum_eq" . ,=)
      ("f64_is_nan" . ,nan?)
      ("f64_is_infinite" . ,inf?)
      ("flonum_to_string" . ,number->string)
      ("string_upcase" . ,string-upcase)
      ("string_downcase" . ,string-downcase)
      ("make_weak_map" . ,make-weak-key-hash-table)
      ("weak_map_get" . ,hash-ref)
      ("weak_map_set" . ,hash-set!)
      ("weak_map_delete" . ,hash-remove!)
      ("fsqrt" . ,sqrt)
      ("fsin" . ,sin)
      ("fcos" . ,cos)
      ("ftan" . ,tan)
      ("fasin" . ,asin)
      ("facos" . ,acos)
      ("fatan" . ,atan)
      ("fatan2" . ,atan)
      ("flog" . ,log)
      ("fexp" . ,exp)
      ("jiffies_per_second" . ,(lambda () internal-time-units-per-second))
      ("current_jiffy" . ,get-internal-real-time)
      ("current_second" . ,(lambda () (exact->inexact (current-time))))
      ("wtf8_to_string" . ,wtf8->string)
      ("string_to_wtf8" . ,string->wtf8)
      ("die" . ,(lambda (key . args)
                  (apply throw (string->symbol key) args)))))
    ("io" .
     (("write_stdout" . ,(lambda (str)
                           (put-string (current-output-port) str)
                           (force-output (current-output-port))))
      ("write_stderr" . ,(lambda (str)
                           (put-string (current-error-port) str)
                           (force-output (current-error-port))))
      ("read_stdin" . ,(lambda () ""))))))

(define (make-abi-imports instance)
  `(("abi" . ,(map (lambda (name)
                     (cons name (wasm-instance-export-ref instance name)))
                   (wasm-instance-export-names instance)))))

(define* (hoot-instantiate scheme-wasm #:optional (imports '())
                           (reflector (force reflect-wasm)))
  (define (debug-str str)
    (format #t "debug: ~a\n" str))
  (define (debug-str-i32 str x)
    (format #t "debug: ~a: ~s\n" str x))
  (define (debug-str-scm str x)
    (format #t "debug: ~a: ~s\n" str (wasm->guile reflector x)))
  (define debug-imports
    `(("debug" .
       (("debug_str" . ,debug-str)
        ("debug_str_i32" . ,debug-str-i32)
        ("debug_str_scm" . ,debug-str-scm)))))
  (define (procedure->extern obj)
    (wasm->guile reflector obj))
  (define ffi-imports
    `(("ffi" .
       (("procedure_to_extern" . ,procedure->extern)))))
  (define (instantiate wasm abi-imports)
    (instantiate-wasm (validate-wasm wasm)
                      #:imports (append imports
                                        abi-imports
                                        debug-imports
                                        ffi-imports)))
  ;; You can either pass an existing reflector and import its ABI, or
  ;; pass a parsed reflection WASM module and create a new reflector.
  (if (reflector? reflector)
      (let* ((imports (append %runtime-imports (reflector-abi reflector)))
             (instance (instantiate scheme-wasm imports)))
        (make-hoot-module reflector instance))
      (let* ((instance (instantiate scheme-wasm %runtime-imports))
             (abi (make-abi-imports instance))
             (imports (append %runtime-imports abi)))
        (set! reflector (make-reflector (instantiate reflector imports) abi))
        (make-hoot-module reflector instance))))

(define (hoot-call reflector f args)
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
                           (loop (+ i 1))))))))))

(define (hoot-load module)
  (match module
    (($ <hoot-module> reflector instance)
     (let* (($load (wasm-instance-export-ref instance "$load")))
       ((wasm->guile reflector (wasm-global-ref $load)))))))

(define reflect-wasm
  (delay
    (call-with-input-file (string-append %hoot-datadir "/js-runtime/reflect.wasm")
      parse-wasm)))

(define* (compile-value exp #:optional (imports '()))
  (hoot-load (hoot-instantiate (compile exp) imports (force reflect-wasm))))

(define (compile-call proc-exp . arg-exps)
  (let* ((proc-module (hoot-instantiate (compile proc-exp) '() (force reflect-wasm)))
         (proc (hoot-load proc-module))
         (reflector (hoot-module-reflector proc-module))
         (args (map (lambda (exp)
                      (hoot-load
                       (hoot-instantiate (compile exp
                                                  #:import-abi? #t
                                                  #:export-abi? #f)
                                         '()
                                         reflector)))
                    arg-exps)))
    (apply proc args)))
