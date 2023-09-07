;;; Hoot standard prelude
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
;;; This file defines the standard environment available to Hoot
;;; programs.  It includes all of R7RS's (scheme base) and a limited
;;; number of extensions.
;;;
;;; Code:

(define-syntax-rule (simple-match e cs ...)
  (let ((v e)) (simple-match-1 v cs ...)))

(define-syntax simple-match-1
  (syntax-rules ()
    ((_ v) (error "value failed to match" v))
    ((_ v (pat e0 e ...) cs ...)
     (let ((fk (lambda () (simple-match-1 v cs ...))))
       (simple-match-pat v pat (let () e0 e ...) (fk))))))

(define-syntax simple-match-pat
  (syntax-rules (_ quote unquote ? and or not)
    ((_ v _ kt kf) kt)
    ((_ v () kt kf) (if (null? v) kt kf))
    ((_ v #t kt kf) (if (eq? v #t) kt kf))
    ((_ v #f kt kf) (if (eq? v #f) kt kf))
    ((_ v (and) kt kf) kt)
    ((_ v (and x . y) kt kf)
     (simple-match-pat v x (simple-match-pat v (and . y) kt kf) kf))
    ((_ v (or) kt kf) kf)
    ((_ v (or x . y) kt kf)
     (let ((tk (lambda () kt)))
       (simple-match-pat v x (tk) (simple-match-pat v (or . y) (tk) kf))))
    ((_ v (not pat) kt kf) (simple-match-pat v pat kf kt))
    ((_ v (quote lit) kt kf)
     (if (equal? v (quote lit)) kt kf))
    ((_ v (? proc) kt kf) (simple-match-pat v (? proc _) kt kf))
    ((_ v (? proc pat) kt kf)
     (if (proc v) (simple-match-pat v pat kt kf) kf))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (simple-match-pat vx x (simple-match-pat vy y kt kf) kf))
         kf))
    ((_ v #(x ...) kt kf)
     (if (and (vector? v)
              (eq? (vector-length v) (length '(x ...))))
         (let ((vv (vector->list v)))
           (simple-match-pat vv (x ...) kt kf))
         kf))
    ((_ v var kt kf) (let ((var v)) kt))))

(define-syntax-rule (match e cs ...) (simple-match e cs ...))

;; Guile extensions.
(define (1+ x) (%+ x 1))
(define (1- x) (%- x 1))

(define (ash x y) (%ash x y))

(define (fold f seed l)
  (let lp ((seed seed) (l l))
    (match l
      (() seed)
      ((x . l) (lp (f x seed) l)))))

(define (fold-right f seed l)
  (let lp ((l l))
    (match l
      (() seed)
      ((x . l) (f x (lp l))))))

(define-syntax-rule (define-associative-eta-expansion f %f)
  (begin
    (define (%generic . args)
      (let lp ((seed (%f)) (args args))
        (match args
          (() seed)
          ((x . args) (lp (%f x seed) args)))))
    (define-syntax f
      (lambda (stx)
        (syntax-case stx ()
          ((_ . x) #'(%f . x))
          (id (identifier? #'id) #'%generic))))))

(define-associative-eta-expansion logand %logand)
(define-associative-eta-expansion logior %logior)
(define-associative-eta-expansion logxor %logxor)

(define (lognot x) (%lognot x))
(define (logtest j k) (%logtest j k))
(define (logbit? idx k) (%logbit? idx k))

(define (keyword? x) (%keyword? x))
(define (bitvector? x) (%bitvector? x))

(define (%generic-cons* head . tail)
  (if (null? tail)
      head
      (cons head (apply %generic-cons* tail))))
(define-syntax cons*
  (lambda (stx)
    (syntax-case stx ()
      ((_) #'(%generic-cons*))
      ((_ a) #'a)
      ((_ a . b) #'(%cons a (cons* . b)))
      (f (identifier? #'f) #'%generic-cons*))))

(define* (make-fluid #:optional default-value)
  (%inline-wasm '(func (param $default (ref eq)) (result (ref eq))
                       (struct.new $fluid (i32.const 0)
                                   (local.get $default)))
                default-value))
(define (fluid-ref x) (%fluid-ref x))
(define (fluid-set! x y) (%fluid-set! x y))
(define (with-fluid* fluid val thunk) (%with-fluid* fluid val thunk))
(define (with-dynamic-state state thunk) (%with-dynamic-state state thunk))

(define-syntax with-fluids
  (lambda (stx)
    (define (emit-with-fluids bindings body)
      (syntax-case bindings ()
        (()
         body)
        (((f v) . bindings)
         #`(with-fluid* f v
             (lambda ()
               #,(emit-with-fluids #'bindings body))))))
    (syntax-case stx ()
      ((_ ((fluid val) ...) exp exp* ...)
       (with-syntax (((fluid-tmp ...) (generate-temporaries #'(fluid ...)))
                     ((val-tmp ...) (generate-temporaries #'(val ...))))
         #`(let ((fluid-tmp fluid) ...)
             (let ((val-tmp val) ...)
               #,(emit-with-fluids #'((fluid-tmp val-tmp) ...)
                                   #'(let () exp exp* ...)))))))))

(define* (make-parameter init #:optional (conv (lambda (x) x)))
  (let ((fluid (make-fluid (conv init))))
    (%inline-wasm
     '(func (param $fluid (ref eq))
            (param $convert (ref eq))
            (result (ref eq))
            (struct.new $parameter
                        (i32.const 0)
                        (ref.func $parameter)
                        (ref.cast $fluid (local.get $fluid))
                        (ref.cast $proc (local.get $convert))))
     fluid conv)))

(define-syntax parameterize
  (lambda (x)
    (syntax-case x ()
      ((_ ((parameter value) ...) body body* ...)
       (with-syntax (((p ...) (generate-temporaries #'(parameter ...))))
         #'(let ((p parameter) ...)
             (define (parameter? x)
               (%inline-wasm
                '(func (param $x (ref eq)) (result (ref eq))
                       (if (ref eq)
                           (ref.test $parameter (local.get $x))
                           (then (i31.new (i32.const 17)))
                           (else (i31.new (i32.const 1)))))
                x))
             (define (parameter-fluid x)
               (%inline-wasm
                '(func (param $param (ref eq)) (result (ref eq))
                       (struct.get $parameter $fluid
                                   (ref.cast $parameter
                                             (local.get $param))))
                x))
             (define (parameter-convert x)
               (%inline-wasm
                '(func (param $param (ref eq)) (result (ref eq))
                       (struct.get $parameter $convert
                                   (ref.cast $parameter
                                             (local.get $param))))
                x))
             (unless (parameter? p)
               (error "not a parameter" p))
             ...
             (with-fluids (((parameter-fluid p) ((parameter-convert p) value))
                           ...)
               body body* ...)))))))

(define (make-atomic-box x) (%make-atomic-box x))
(define (atomic-box-ref x) (%atomic-box-ref x))
(define (atomic-box-set! x y) (%atomic-box-set! x y))
(define (atomic-box-swap! x y) (%atomic-box-swap! x y))
(define (atomic-box-compare-and-swap! x y z) (%atomic-box-compare-and-swap! x y z))

(define-syntax-rule (define-primcall f %f arg ...)
  (begin
    (define (generic arg ...)
      (%f arg ...))
    (define-syntax f
      (lambda (stx)
        (syntax-case stx ()
          ((_ . x) #'(%f . x))
          (id (identifier? #'id) #'generic))))))
(define-primcall call-with-prompt %call-with-prompt tag body handler)
(define* (make-prompt-tag #:optional (stem "prompt"))
  (list stem))

(define (call-with-current-continuation f)
  ;; FIXME: Implement delimited call/cc.
  (error "unimplemented"))
(define call/cc call-with-current-continuation)

(define (dynamic-wind wind body unwind)
  (%dynamic-wind wind body unwind))

(define-syntax call-with-values
  (lambda (stx)
    (syntax-case stx (lambda)
      ((_ producer (lambda args body0 body ...))
       #'(%call-with-values producer (lambda args body0 body ...)))
      (id (identifier? #'id)
          #'(lambda (producer consumer)
              (let ((p producer) (c consumer))
                (%call-with-values p (lambda args (apply c args)))))))))

(define (pair? p) (%pair? p))
(define (cons x y) (%cons x y))
(define (car x) (%car x))
(define (cdr x) (%cdr x))
(define (caar x) (%car (%car x)))
(define (cadr x) (%car (%cdr x)))
(define (cdar x) (%cdr (%car x)))
(define (cddr x) (%cdr (%cdr x)))
(define (caaar x) (%car (%car (%car x))))
(define (cadar x) (%car (%cdr (%car x))))
(define (caadr x) (%car (%car (%cdr x))))
(define (caddr x) (%car (%cdr (%cdr x))))
(define (cdaar x) (%cdr (%car (%car x))))
(define (cddar x) (%cdr (%cdr (%car x))))
(define (cdadr x) (%cdr (%car (%cdr x))))
(define (cdddr x) (%cdr (%cdr (%cdr x))))
(define (caaaar x) (%car (%car (%car (%car x)))))
(define (caadar x) (%car (%car (%cdr (%car x)))))
(define (caaadr x) (%car (%car (%car (%cdr x)))))
(define (caaddr x) (%car (%car (%cdr (%cdr x)))))
(define (cadaar x) (%car (%cdr (%car (%car x)))))
(define (caddar x) (%car (%cdr (%cdr (%car x)))))
(define (cadadr x) (%car (%cdr (%car (%cdr x)))))
(define (cadddr x) (%car (%cdr (%cdr (%cdr x)))))
(define (cdaaar x) (%cdr (%car (%car (%car x)))))
(define (cdadar x) (%cdr (%car (%cdr (%car x)))))
(define (cdaadr x) (%cdr (%car (%car (%cdr x)))))
(define (cdaddr x) (%cdr (%car (%cdr (%cdr x)))))
(define (cddaar x) (%cdr (%cdr (%car (%car x)))))
(define (cdddar x) (%cdr (%cdr (%cdr (%car x)))))
(define (cddadr x) (%cdr (%cdr (%car (%cdr x)))))
(define (cddddr x) (%cdr (%cdr (%cdr (%cdr x)))))

(define (set-car! x y) (%set-car! x y))
(define (set-cdr! x y) (%set-cdr! x y))

(define* (%debug message #:optional (val '(missing)))
  (cond
   ((eq? val '(missing))
    (%inline-wasm
     '(func (param $str (ref eq))
            (call $debug-str
                  (struct.get $string $str
                              (ref.cast $string (local.get $str)))))
     message))
   (else
    (%inline-wasm
     '(func (param $str (ref eq)) (param $val (ref eq))
            (call $debug-str-scm
                  (struct.get $string $str
                              (ref.cast $string (local.get $str)))
                  (local.get $val)))
     message val))))

(define* (pk v . v*)
  (match (reverse (cons v v*))
    ((val . vals)
     (for-each (lambda (v) (%debug "pk_" v)) (reverse vals))
     (%debug "pkv" val)
     val)))

(define (length l)
  (let lp ((len 0) (l l))
    (if (null? l) len (lp (1+ len) (cdr l)))))
(define (list . args) args)

(define (list-ref l n)
  (let lp ((l l) (n n))
    (if (zero? n)
        (car l)
        (lp (cdr l) (1- n)))))
(define (list-set! l n x)
  (let lp ((l l) (n n))
    (if (zero? n)
        (set-car! l x)
        (lp (cdr l) (1- n)))))
(define (list-tail l n)
  (let lp ((l l) (n n))
    (if (zero? n)
        l
        (lp (cdr l) (1- n)))))
(define (list? l)
  (let lp ((l l))
    (match l
      (() #t)
      ((_ . l) (lp l))
      (_ #f))))
(define (make-list n init)
  (let lp ((n n) (out '()))
    (if (zero? n)
        out
        (lp (1- n) (cons init out)))))
(define (null? x) (%null? x))

(define (reverse l)
  (let lp ((out '()) (l l))
    (match l
      (() out)
      ((head . tail) (lp (cons head out) tail)))))
(define (append . args)
  (match args
    (() '())
    ((l) l)
    ((l1 l2)
     (fold-right cons l2 l1))
    ((l1 . l*)
     (append l1 (apply append l*)))))
(define (list-copy l)
  (append l '()))

(define-syntax-rule (define-member+assoc member assoc compare optarg ...)
  (begin
    (define* (member v l optarg ...)
      (let lp ((l l))
        (cond
         ((null? l) #f)
         ((compare v (car l)) #t)
         (else (lp (cdr l))))))
    (define* (assoc v l optarg ...)
      (let lp ((l l))
        (and (not (null? l))
             (let ((head (car l)))
               (if (compare v head)
                   head
                   (lp (cdr l)))))))))
(define-member+assoc memq assq eq?)
(define-member+assoc memv assv eqv?)
(define-member+assoc member assoc compare #:optional (compare equal?))

(define* (make-bytevector len #:optional (init 0))
  (unless (and (exact-integer? len) (= len (logand len (1- (ash 1 29)))))
    ;; FIXME: Allow bigint len ?
    (error "expected length to be integer in range [0,2^29-1]" len))
  (unless (and (exact-integer? init) (<= -128 init 255))
    (error "expected init to be integer in range [-128, 255]" init))
  (%inline-wasm
   '(func (param $len (ref eq)) (param $init (ref eq))
          (result (ref eq))
          (struct.new
           $mutable-bytevector
           (i32.const 0)
           (array.new
            $raw-bytevector
            (i32.shr_s (i31.get_s (ref.cast i31 (local.get $init)))
                       (i32.const 1))
            (i32.shr_s (i31.get_s (ref.cast i31 (local.get $len)))
                       (i32.const 1)))))
   len init))
(define (bytevector-length bv) (%bytevector-length bv))
(define (bytevector-u8-ref bv i)     (%bytevector-u8-ref bv i))
(define (bytevector-u8-set! bv i x)  (%bytevector-u8-set! bv i x))
(define (bytevector-s8-ref bv i)     (%bytevector-s8-ref bv i))
(define (bytevector-s8-set! bv i x)  (%bytevector-s8-set! bv i x))
(define (bytevector-u16-native-ref bv i)    (%bytevector-u16-native-ref bv i))
(define (bytevector-u16-native-set! bv i x) (%bytevector-u16-native-set! bv i x))
(define (bytevector-s16-native-ref bv i)    (%bytevector-s16-native-ref bv i))
(define (bytevector-s16-native-set! bv i x) (%bytevector-s16-native-set! bv i x))
(define (bytevector-u32-native-ref bv i)    (%bytevector-u32-native-ref bv i))
(define (bytevector-u32-native-set! bv i x) (%bytevector-u32-native-set! bv i x))
(define (bytevector-s32-native-ref bv i)    (%bytevector-s32-native-ref bv i))
(define (bytevector-s32-native-set! bv i x) (%bytevector-s32-native-set! bv i x))
(define (bytevector-u64-native-ref bv i)    (%bytevector-u64-native-ref bv i))
(define (bytevector-u64-native-set! bv i x) (%bytevector-u64-native-set! bv i x))
(define (bytevector-s64-native-ref bv i)    (%bytevector-s64-native-ref bv i))
(define (bytevector-s64-native-set! bv i x) (%bytevector-s64-native-set! bv i x))
(define (bytevector-ieee-single-native-ref bv i)    (%bytevector-ieee-single-native-ref bv i))
(define (bytevector-ieee-single-native-set! bv i x) (%bytevector-ieee-single-native-set! bv i x))
(define (bytevector-ieee-double-native-ref bv i)    (%bytevector-ieee-double-native-ref bv i))
(define (bytevector-ieee-double-native-set! bv i x) (%bytevector-ieee-double-native-set! bv i x))
(define (bytevector? x) (%bytevector? x))
(define (bytevector . inits)
  (let* ((len (length inits))
         (bv (make-bytevector len)))
    (let lp ((i 0) (inits inits))
      (when (< i len)
        (bytevector-u8-set! bv i (car inits))
        (lp (1+ i) (cdr inits))))
    bv))
(define (bytevector-concatenate bv*)
  (match bv*
    (() #vu8())
    ((bv) bv)
    (bv*
     (let* ((len (fold (lambda (bv len) (+ (bytevector-length bv) len)) 0 bv*))
            (flattened (make-bytevector len 0)))
       (let lp ((bv* bv*) (cur 0))
         (match bv*
           (() flattened)
           ((bv . bv*)
            (bytevector-copy! flattened cur bv)
            (lp bv* (+ cur (bytevector-length bv))))))))))
(define (bytevector-concatenate-reverse bv*)
  (match bv*
    (() #vu8())
    ((bv) bv)
    (bv*
     (let* ((len (fold (lambda (bv len) (+ (bytevector-length bv) len)) 0 bv*))
            (flattened (make-bytevector len 0)))
       (let lp ((bv* bv*) (cur len))
         (match bv*
           (() flattened)
           ((bv . bv*)
            (let ((cur (- cur (bytevector-length bv))))
              (bytevector-copy! flattened cur bv)
              (lp bv* cur)))))))))
(define (bytevector-append . args)
  (bytevector-concatenate args))
(define* (bytevector-copy x #:optional (start 0) (end (bytevector-length x)))
  (unless (and (exact-integer? start) (<= 0 start (bytevector-length x)))
    (error "bad start" start))
  (unless (and (exact-integer? end) (<= start end (bytevector-length x)))
    (error "bad end" end))
  (%inline-wasm
   '(func (param $src (ref eq)) (param $start (ref eq)) (param $end (ref eq))
          (result (ref eq))
          (local $i0 i32)
          (local $i1 i32)
          (local $vu0 (ref $raw-bytevector))
          (local.set $i0 (i32.shr_u
                          (i31.get_u (ref.cast i31 (local.get $start)))
                          (i32.const 1)))
          (local.set $i1 (i32.sub
                          (i32.shr_u
                           (i31.get_u (ref.cast i31 (local.get $end)))
                           (i32.const 1))
                          (local.get $i0)))
          (local.set $vu0 (array.new_default $raw-bytevector (local.get $i1)))
          (array.copy $raw-bytevector $raw-bytevector
                      (local.get $vu0) (i32.const 0)
                      (struct.get $bytevector $vals
                                  (ref.cast $bytevector (local.get $src)))
                      (local.get $i0) (local.get $i1))
          (struct.new $bytevector (i32.const 0) (local.get $vu0)))
   x start end))
(define* (bytevector-copy! to at from #:optional
                           (start 0) (end (bytevector-length from)))
  (unless (and (exact-integer? at) (<= 0 at (bytevector-length to)))
    (error "bad at" at))
  (unless (and (exact-integer? start) (<= 0 start (bytevector-length from)))
    (error "bad start" start))
  (unless (and (exact-integer? end) (<= start end (bytevector-length from)))
    (error "bad end" end))
  (%inline-wasm
   '(func (param $to (ref eq)) (param $at (ref eq))
          (param $from (ref eq)) (param $start (ref eq)) (param $end (ref eq))
          (array.copy $raw-bytevector $raw-bytevector
                      (struct.get $bytevector $vals
                                  (ref.cast $bytevector (local.get $to)))
                      (i32.shr_u
                       (i31.get_u (ref.cast i31 (local.get $at)))
                       (i32.const 1))
                      (struct.get $bytevector $vals
                                  (ref.cast $bytevector (local.get $from)))
                      (i32.shr_u
                       (i31.get_u (ref.cast i31 (local.get $start)))
                       (i32.const 1))
                      (i32.shr_u
                       (i32.sub
                        (i31.get_u (ref.cast i31 (local.get $end)))
                        (i31.get_u (ref.cast i31 (local.get $start))))
                       (i32.const 1))))
   to at from start end))

(define-associative-eta-expansion * %*)
(define-associative-eta-expansion + %+)

(define-syntax-rule (define-sub/div-eta-expansion f %f zero)
  (begin
    (define (%generic y . tail)
      (if (null? tail)
          (%f zero y)
          (let lp ((y y) (tail tail))
            (let ((y (%f y (car tail)))
                  (tail (cdr (tail))))
              (if (null? tail)
                  y
                  (lp y tail))))))
    (define-syntax f
      (lambda (stx)
        (syntax-case stx ()
          ((_ . x) #'(%f . x))
          (f (identifier? #'f) #'%generic))))))

(define-sub/div-eta-expansion - %- 0)
(define-sub/div-eta-expansion / %/ 1)

(define-syntax-rule (define-comparison-expansion f %f)
  (begin
    (define (%generic x y . tail)
      (and (%f x y)
           (or (null? tail)
               (apply %generic y tail))))
    (define-syntax f
      (lambda (stx)
        (syntax-case stx ()
          ((_ x y . z) #'(%f x y . z))
          (f (identifier? #'f) #'%generic))))))

(define-comparison-expansion < %<)
(define-comparison-expansion <= %<=)
(define-comparison-expansion = %=)
(define-comparison-expansion >= %>=)
(define-comparison-expansion > %>)

(define (abs x) (%abs x))
(define (floor x) (%floor x))
(define (ceiling x) (%ceiling x))
(define (round x) (%floor (+ x 0.5)))
(define (number? x) (%number? x))
(define (complex? x) (%complex? x))
(define (real? x) (%real? x))
(define (rational? x) (%rational? x))
(define (integer? x) (%integer? x))
(define (exact-integer? x) (%exact-integer? x))
(define (exact? x) (%exact? x))
(define (inexact? x) (%inexact? x))

(define (inexact x) (%inexact x))
(define (exact x) (error "unimplemented"))

(define (quotient x y) (%quotient x y))
(define (remainder x y) (%remainder x y))
(define (modulo x y) (%modulo x y))

(define (even? x) (zero? (logand x 1)))
(define (odd? x) (not (even? x)))

(define (numerator x) (error "unimplemented"))
(define (denominator x) (error "unimplemented"))
(define (exact-integer-sqrt x) (error "unimplemented"))

(define (floor/ x y) (error "unimplemented"))
(define (floor-quotient x y) (error "unimplemented"))
(define (floor-remainder x y) (error "unimplemented"))

(define (truncate/ x y) (error "unimplemented"))
(define (truncate-quotient x y) (error "unimplemented"))
(define (truncate-remainder x y) (error "unimplemented"))

(define (%binary-gcd x y) (error "unimplemented"))
(define-syntax %gcd
  (syntax-rules ()
    ((_) 0)
    ((_ x) x)
    ((_ x y) (%binary-gcd x y))))

(define (%binary-lcm x y) (error "unimplemented"))
(define-syntax %lcm
  (syntax-rules ()
    ((_) 1)
    ((_ x) x)
    ((_ x y) (%binary-lcm x y))))

(define-associative-eta-expansion gcd %gcd)
(define-associative-eta-expansion lcm %lcm)

(define (max x . y)
  (match y
    (() x)
    ((y . y*) (apply max (if (> x y) x y) y*))))
(define (min x . y)
  (match y
    (() x)
    ((y . y*) (apply min (if (< x y) x y) y*))))

(define (negative? x) (< x 0))
(define (positive? x) (> x 0))
(define (zero? x) (= x 0))

(define (sin x) (%sin x))
(define (cos x) (%cos x))
(define (tan x) (%tan x))
(define (asin x) (%asin x))
(define (acos x) (%acos x))
;; FIXME: optargs
(define (%generic-atan x . y)
  (if (null? y)
      (%atan x)
      (%atan x (car y))))
(define-syntax atan
  (lambda (stx)
    (syntax-case stx ()
      ((_ x) #'(%atan x))
      ((_ x y) #'(%atan x y))
      (f (identifier? #'f) #'%generic-atan))))
(define (sqrt x) (%sqrt x))
(define* (log x #:optional y) (error "unimplemented"))
(define (exp x) (error "unimplemented"))

(define (number->string n) (error "unimplemented"))
(define (string->number n) (error "unimplemented"))

(define (rationalize x y) (error "unimplemented"))
(define (square x) (* x x))
(define (expt x y) (error "unimplemented"))
   
;; (scheme complex)
(define (make-polar real imag) (error "unimplemented"))
(define (magnitude z) (error "unimplemented"))
(define (angle z) (error "unimplemented"))
(define (real-part z) (error "unimplemented"))
(define (imag-part z) (error "unimplemented"))

;; promises
(define (make-promise x) (error "unimplemented"))
(define (promise? x) (error "unimplemented"))
(define (force x) (error "unimplemented"))
(define-syntax-rule (delay expr) (make-promise (lambda () expr)))
;; FIXME: implement properly.
(define-syntax-rule (delay-force expr) (delay (force expr)))

(define (char->integer x) (%char->integer x))
(define (integer->char x) (%integer->char x))
(define (char? x) (%char? x))
(define (char<? . args) (apply < (map char->integer args)))
(define (char<=? . args) (apply <= (map char->integer args)))
(define (char=? . args) (apply = (map char->integer args)))
(define (char>=? . args) (apply >= (map char->integer args)))
(define (char>? . args) (apply > (map char->integer args)))

;; (scheme inexact)
(define (finite? z)
  (and (not (infinite? z))
       (not (nan? z))))
(define (infinite? z)
  (if (complex? z)
      (or (%inf? (real-part z))
          (%inf? (imag-part z)))
      (%inf? z)))
(define (nan? z)
  (if (complex? z)
      (or (%nan? (real-part z))
          (%nan? (imag-part z)))
      (%nan? z)))

;; (scheme char) procedures; mostly we should punt to ICU via the host
(define (char-downcase char) (error "unimplemented"))
(define (char-upcase char) (error "unimplemented"))
(define (char-foldcase char)
  (if (or (eqv? char #\460) (eqv? char #\461))
      char
      (char-downcase (char-upcase char))))
(define (digit-value char) (error "unimplemented"))
(define (char-alphabetic? char) (error "unimplemented"))
(define (char-ci<? ch1 ch2 . ch*) (error "unimplemented"))
(define (char-ci<=? ch1 ch2 . ch*) (error "unimplemented"))
(define (char-ci=? ch1 ch2 . ch*) (error "unimplemented"))
(define (char-ci>=? ch1 ch2 . ch*) (error "unimplemented"))
(define (char-ci>? ch1 ch2 . ch*) (error "unimplemented"))
(define (char-lower-case? char) (error "unimplemented"))
(define (char-upper-case? char) (error "unimplemented"))
(define (char-whitespace? char) (error "unimplemented"))
(define (char-numeric? char) (error "unimplemented"))
(define (string-upcase str) (error "unimplemented"))
(define (string-downcase str) (error "unimplemented"))
(define (string-foldcase str) (error "unimplemented"))

(define (make-box init) (%make-box init))
(define (box-ref box) (%box-ref box))
(define (box-set! box val) (%box-set! box val))

;; FIXME: kwargs
;; FIXME: suspendability
(define (%make-port read
                    write
                    input-waiting?
                    seek
                    close
                    truncate
                    repr
                    file-name
                    read-buf-size
                    write-buf-size
                    r/w-random-access?
                    private-data)
  (when file-name
    (unless (string? file-name) (error "bad file name" file-name)))
  (when read
    (unless (and (exact-integer? read-buf-size) (< 0 read-buf-size))
      (error "bad read buf size" read-buf-size)))
  (when write
    (unless (and (exact-integer? write-buf-size) (< 0 write-buf-size))
      (error "bad write buf size" write-buf-size)))
  (unless (string? repr)
    (error "missing repr" repr))
  (let ((read-buf (and read (vector (make-bytevector read-buf-size 0) 0 0 #f)))
        (write-buf (and write (vector (make-bytevector write-buf-size 0) 0 0))))
    (%inline-wasm
     '(func (param $read (ref eq))
            (param $write (ref eq))
            (param $seek (ref eq))
            (param $input-waiting? (ref eq))
            (param $close (ref eq))
            (param $truncate (ref eq))
            (param $repr (ref eq))
            (param $file-name (ref eq))
            (param $read-buf (ref eq))
            (param $write-buf (ref eq))
            (param $read-buffering (ref eq))
            (param $r/w-random-access? (ref eq))
            (param $private-data (ref eq))
            (result (ref eq))
            (struct.new $port (i32.const 0)
                        (local.get $read)
                        (local.get $write)
                        (local.get $input-waiting?)
                        (local.get $seek)
                        (local.get $close)
                        (local.get $truncate)
                        (ref.cast $string (local.get $repr))
                        (local.get $file-name)
                        (struct.new $mutable-pair
                                    (i32.const 0)
                                    (i31.new (i32.const 0))
                                    (i31.new (i32.const 0)))
                        (local.get $read-buf)
                        (local.get $write-buf)
                        (local.get $read-buffering)
                        (local.get $r/w-random-access?)
                        (local.get $private-data)))
     read write input-waiting? seek close truncate repr file-name
     read-buf write-buf read-buf-size r/w-random-access? private-data)))

(define (%set-port-buffer-cur! buf cur)           (vector-set! buf 1 cur))
(define (%set-port-buffer-end! buf end)           (vector-set! buf 2 end))
(define (%set-port-buffer-has-eof?! buf has-eof?) (vector-set! buf 3 has-eof?))

(define-syntax-rule (%define-simple-port-getter getter $field)
  (define (getter port)
    ;; FIXME: arg type checking
    (%inline-wasm
     '(func (param $port (ref eq)) (result (ref eq))
            (struct.get $port $field (ref.cast $port (local.get $port))))
     port)))
(define-syntax-rule (%define-simple-port-setter setter $field)
  (define (setter port val)
    ;; FIXME: arg type checking
    (%inline-wasm
     '(func (param $port (ref eq)) (param $val (ref eq))
            (struct.set $port $field (ref.cast $port (local.get $port))
                        (local.get $val)))
     port val)))

(%define-simple-port-getter %port-read $read)
(%define-simple-port-getter %port-write $write)
(%define-simple-port-getter %port-seek $seek)
(%define-simple-port-getter %port-close $close)
(%define-simple-port-getter %port-truncate $truncate)
(%define-simple-port-getter %port-repr $repr)
(%define-simple-port-getter %port-file-name $file-name)
(%define-simple-port-getter %port-position $position)
(%define-simple-port-getter %port-read-buffer $read-buf)
(%define-simple-port-getter %port-write-buffer $write-buf)
(%define-simple-port-getter %port-read-buffering $read-buffering)
(%define-simple-port-getter %port-r/w-random-access? $r/w-random-access?)
(%define-simple-port-getter %port-private-data $private-data)

(%define-simple-port-setter %set-port-file-name! $file-name)
(%define-simple-port-setter %set-port-read-buffer! $read-buf)
(%define-simple-port-setter %set-port-write-buffer! $write-buf)
(%define-simple-port-setter %set-port-read-buffering! $read-buffering)

(define* (get-output-bytevector port #:optional (clear-buffer? #f))
  ;; FIXME: How to know it's a bytevector output port?
  (define accum (%port-private-data port))
  (flush-output-port port)
  (let ((flattened (bytevector-concatenate (box-ref accum))))
    (box-set! accum (if clear-buffer?
                        '()
                        (list flattened)))
    flattened))

(define (open-output-bytevector)
  (define accum (make-box '()))
  (define pos #f)
  (define (appending?) (not pos))
  (define default-buffer-size 1024)
  (define (bv-write bv start count)  ; write
    (unless (zero? count)
      (cond
       ((appending?)
        (box-set! accum
                  (cons (bytevector-copy bv start (+ start count))
                        (box-ref accum))))
       (else
        (let* ((dst (get-output-bytevector port))
               (to-copy (min count (- (bytevector-length dst) pos))))
          (bytevector-copy! dst pos bv start to-copy)
          (cond
           ((< to-copy count)
            (box-set!
             accum
             (list (bytevector-copy bv (+ start to-copy) (- count to-copy))
                   dst))
            (set! pos #f))
           (else
            (set! pos (+ pos count))))))))
    count)
  (define (bv-seek offset whence)    ; seek
    (define len (bytevector-length (get-output-bytevector port)))
    (define base (match whence ('start 0) ('cur (or pos len)) ('end len)))
    (define dst (+ base offset))
    (when (or (< dst 0) (< len dst))
      (error "out of range" offset))
    (set! pos (if (= pos dst) #f dst))
    dst)

  (define port
    (%make-port #f                      ; read
                bv-write
                #f                      ; input-waiting?
                bv-seek
                #f                      ; close
                #f                      ; truncate
                "bytevector"            ; repr
                #f                      ; file-name
                #f                      ; read-buf-size
                default-buffer-size     ; write-buf-size
                #f                      ; r/w-random-access
                accum                   ; private data
                ))
  port)

(define (open-input-bytevector src)
  (define pos 0)
  (define default-buffer-size 1024)
  (define (bv-read dst start count)
    (let* ((to-copy (min count (- (bytevector-length src) pos)))
           (end (+ pos to-copy)))
      (bytevector-copy! dst start src pos end)
      (set! pos end)
      to-copy))
  (define (bv-seek offset whence)    ; seek
    (define len (bytevector-length src))
    (define base (match whence ('start 0) ('cur len) ('end len)))
    (define dst (+ base offset))
    (when (or (< dst 0) (< len dst))
      (error "out of range" offset))
    (set! pos dst)
    dst)
  (%make-port bv-read
              #f                      ; write
              #f                      ; input-waiting?
              bv-seek
              #f                      ; close
              #f                      ; truncate
              "bytevector"            ; repr
              #f                      ; file-name
              default-buffer-size     ; read-buf-size
              #f                      ; write-buf-size
              #f                      ; r/w-random-access
              #f                      ; private data
              ))

(define (open-input-string str)
  (open-input-bytevector (string->utf8 str)))

(define (open-output-string) (open-output-bytevector))
(define* (get-output-string p #:optional (clear-buffer? #f))
  (utf8->string (get-output-bytevector p clear-buffer?)))

;; R7RS ports
(define (eof-object? x) (%eof-object? x))
(define (eof-object)
  (define-syntax eof-object
    (lambda (stx) #`'#,%the-eof-object))
  (eof-object))

(define (port? x) (error "unimplemented"))
(define (input-port? x) (error "unimplemented"))
(define (output-port? x) (error "unimplemented"))
(define (binary-port? x) (port? x))
(define (textual-port? x) (port? x))
(define (input-port-open? x) (error "unimplemented"))
(define (output-port-open? x) (error "unimplemented"))
(define (close-input-port x)
  (unless (input-port? x) (error "not an input port"))
  (close-port x))
(define (close-output-port x)
  (unless (output-port? x) (error "not an output port"))
  (close-port x))
(define (close-port x) (error "unimplemented"))

(define (seek port offset whence)
  (error "unimplemented"))

(define (%write-bytes port bv start count)
  (let ((written ((%port-write port) bv start count)))
    (unless (<= 0 written count)
      (error "bad return from port write function" written))
    (when (< written count)
      (%write-bytes port bv (+ start written) (- count written)))))

(define (%read-bytes port bv start count)
  (let ((read ((%port-read port) bv start count)))
    (unless (<= 0 read count)
      (error "bad return from port read function" read))
    read))

(define* (flush-input-port #:optional (port (current-output-port)))
  ;; For buffered input+output ports that are random-access?, it's
  ;; likely that when switching from reading to writing that we will
  ;; have some bytes waiting to be read, and that the underlying
  ;; port-position is ahead.  This function discards buffered input and
  ;; seeks back from before the buffered input.
  (match (%port-read-buffer port)
    (#f (error "not an output port"))
    ((and buf #(bv cur end has-eof?))
     (when (< cur end)
       (%set-port-buffer-cur! buf 0)
       (%set-port-buffer-end! buf 0)
       (seek port (- cur end) 'cur)))))
(define* (flush-output-port #:optional (port (current-output-port)))
  (match (%port-write-buffer port)
    (#f (error "not an output port"))
    ((and buf #(bv cur end))
     (when (< cur end)
       (%set-port-buffer-cur! buf 0)
       (%set-port-buffer-end! buf 0)
       (%write-bytes port bv cur (- end cur))))))

(define* (u8-ready? #:optional (port (current-input-port)))
  (error "unimplemented"))

(define (%fill-input port buf minimum-buffering)
  (match buf
    (#(bv cur end has-eof?)
     (let ((avail (- end cur)))
       (cond
        ((or has-eof?
             (<= minimum-buffering avail))
         (values buf avail))
        ((< (bytevector-length bv) minimum-buffering)
         (let* ((expanded (make-bytevector minimum-buffering 0))
                (buf (vector expanded 0 (- end cur) #f)))
           (when (< cur end)
             (bytevector-copy! expanded 0 bv cur end))
           (%set-port-read-buffer! port buf)
           (%fill-input port buf minimum-buffering)))
        (else
         (when (< 0 cur)
           (bytevector-copy! bv 0 bv cur end)
           (%set-port-buffer-cur! buf 0))
         (let lp ((end avail))
           (let* ((must-read (- minimum-buffering end))
                  ;; precondition: read-buffering <= len(read-buffer)
                  ;; precondition: minimum-buffering <= len(read-buffer)
                  ;; precondition: end < minimum-buffering
                  (count (- (max (%port-read-buffering port)
                                 minimum-buffering)
                            end))
                  (read (%read-bytes port bv end count))
                  (end (+ end read)))
             (cond
              ((zero? read)
               (%set-port-buffer-end! buf end)
               (%set-port-buffer-has-eof?! buf #t)
               (values buf end))
              ((< end minimum-buffering)
               (lp end))
              (else
               (%set-port-buffer-end! buf end)
               (values buf end)))))))))))

(define* (peek-u8 #:optional (port (current-input-port)))
  (let lp ((buf (%port-read-buffer port)))
    (match buf
      (#f (error "not an input port"))
      (#(bv cur end has-eof?)
       (cond
        ((eq? cur end)
         (if has-eof?
             (eof-object)
             (call-with-values (lambda ()
                                 (%fill-input port buf 1))
               (lambda (buf avail)
                 (if (zero? avail)
                     (eof-object)
                     (lp buf))))))
        (else
         (bytevector-u8-ref bv cur)))))))

(define* (read-u8 #:optional (port (current-input-port)))
  (define (read-eof! buf)
    (%set-port-buffer-has-eof?! buf #f)
    (eof-object))
  (let lp ((buf (%port-read-buffer port)))
    (match buf
      (#f (error "not an input port"))
      (#(bv cur end has-eof?)
       (cond
        ((eq? cur end)
         (if has-eof?
             (read-eof! buf)
             (call-with-values (lambda ()
                                 (%fill-input port buf 1))
               (lambda (buf avail)
                 (if (zero? avail)
                     (read-eof! buf)
                     (lp buf))))))
        (else
         (%set-port-buffer-cur! buf (1+ cur))
         (bytevector-u8-ref bv cur)))))))

(define* (read-bytevector k #:optional (port (current-input-port)))
  (unless (and (exact-integer? k) (<= 0 k))
    (error "read-bytevector: bad k" k))
  (call-with-values (lambda ()
                      (%fill-input port (%port-read-buffer port) (max k 1)))
    (lambda (buf avail)
      (cond
       ((zero? avail)
        (%set-port-buffer-has-eof?! buf #f)
        (eof-object))
       (else
        (match buf
          (#(src cur end has-eof?)
           (let* ((cur* (min (+ cur k) end))
                  (bv (bytevector-copy src cur cur*)))
             (%set-port-buffer-cur! buf cur*)
             bv))))))))

(define* (read-bytevector! dst #:optional (port (current-input-port))
                           (start 0) (end (bytevector-length dst)))
  (unless (and (exact-integer? start) (<= 0 start (bytevector-length dst)))
    (error "bad start" start))
  (unless (and (exact-integer? end) (<= start end (bytevector-length dst)))
    (error "bad end" end))
  (let ((count (- start end)))
    (call-with-values (lambda ()
                        (%fill-input port (%port-read-buffer port)
                                     (max count 1)))
      (lambda (buf avail)
        (cond
         ((zero? avail)
          (%set-port-buffer-has-eof?! buf #f)
          (eof-object))
         (else
          (match buf
            (#(src cur end has-eof?)
             (let* ((cur* (min (+ cur count) end))
                    (count (- cur* cur)))
               (bytevector-copy! dst start src cur cur*)
               (%set-port-buffer-cur! buf cur*)
               count)))))))))

(define* (char-ready? #:optional (port (current-input-port)))
  (error "unimplemented"))

(define* (peek-char #:optional (port (current-input-port)))
  (let ((a (peek-u8 port)))
    (cond
     ((eof-object? a) a)
     ((< a #b10000000) (integer->char a))
     (else
      ;; FIXME: This is a sloppy UTF-8 decoder.  Need to think more
      ;; about this.
      (let ((len (cond ((< a #b11100000) 2)
                       ((< a #b11110000) 3)
                       (else 4))))
        (call-with-values (lambda ()
                            (%fill-input port (%port-read-buffer port) len))
          (lambda (buf avail)
            (when (< len avail)
              (error "decoding error: partial utf-8 sequence"))
            (match buf
              (#(bv cur end has-eof?)
               (%inline-wasm
                '(func (param $bv (ref eq))
                       (param $cur (ref eq))
                       (param $end (ref eq))
                       (result (ref eq))
                       (i31.new
                        (stringview_iter.next
                         (string.as_iter
                          (string.new_lossy_utf8_array
                           (struct.get $bytevector $vals
                                       (ref.cast $bytevector (local.get $bv)))
                           (i32.shr_s
                            (i31.get_s (ref.cast i31 (local.get $cur)))
                            (i32.const 1))
                           (i32.shr_s
                            (i31.get_s (ref.cast i31 (local.get $end)))
                            (i32.const 1)))))))
                bv cur (+ cur len)))))))))))

(define* (read-char #:optional (port (current-input-port)))
  (let ((a (peek-u8 port)))
    (cond
     ((eof-object? a) a)
     ((<= a #x7f)
      (match (%port-read-buffer port)
        ((and buf #(bv cur end has-eof?))
         (%set-port-buffer-cur! buf (1+ cur))
         (integer->char a))))
     (else
      (let ((len (cond ((< a #b11100000) 2)
                       ((< a #b11110000) 3)
                       (else 4))))
        (call-with-values (lambda ()
                            (%fill-input port (%port-read-buffer port) len))
          (lambda (buf avail)
            (when (< len avail)
              (error "decoding error: partial utf-8 sequence"))
            (match buf
              (#(bv cur end has-eof?)
               (%set-port-buffer-cur! buf (+ cur len))
               (%inline-wasm
                '(func (param $bv (ref eq))
                       (param $cur (ref eq))
                       (param $end (ref eq))
                       (result (ref eq))
                       (i31.new
                        (stringview_iter.next
                         (string.as_iter
                          (string.new_lossy_utf8_array
                           (struct.get $bytevector $vals
                                       (ref.cast $bytevector (local.get $bv)))
                           (i32.shr_s
                            (i31.get_s (ref.cast i31 (local.get $cur)))
                            (i32.const 1))
                           (i32.shr_s
                            (i31.get_s (ref.cast i31 (local.get $end)))
                            (i32.const 1)))))))
                bv cur (+ cur len)))))))))))
(define* (read-string k #:optional (port (current-input-port)))
  (cond
   ;; Call peek-char to ensure we're at the start of some UTF-8.
   ((eof-object? (peek-char port)) (eof-object))
   (else
    (match (%port-read-buffer port)
      ((and buf #(bv cur end has-eof?))
       (define (take-string count cur*)
         (%set-port-buffer-cur! buf cur*)
         (define str (utf8->string bv cur cur*))
         (let ((remaining (- k count)))
           (if (zero? remaining)
               str
               (match (read-string remaining port)
                 ((? eof-object?) str)
                 (tail (string-append str tail))))))
       ;; Count codepoints in buffer.
       (let count-codepoints ((count 0) (cur cur))
         (if (and (< cur end) (< count k))
             (let* ((u8 (bytevector-u8-ref bv cur))
                    (len (cond ((< u8 #b10000000) 1)
                               ((< u8 #b11100000) 2)
                               ((< u8 #b11110000) 3)
                               (else 4))))
               (if (<= (+ cur len) end)
                   (count-codepoints (1+ count) (+ cur len))
                   (take-string count cur)))
             (take-string count cur))))))))
(define* (read-line #:optional (port (current-input-port)))
  (define bytes '())
  (define (finish)
    (utf8->string (bytevector-concatenate-reverse bytes)))
  (let read-some ((buf (%port-read-buffer port)))
    (match buf
      (#(bv cur end has-eof?)
       (define (accumulate-bytes! end)
         (set! bytes (cons (bytevector-copy bv cur end) bytes)))
       (let scan-for-newline ((pos cur))
         (cond
          ((< pos end)
           (let ((u8 (bytevector-u8-ref bv pos)))
             (cond
              ((or (eq? u8 (char->integer #\newline))
                   (eq? u8 (char->integer #\return)))
               (accumulate-bytes! pos)
               (%set-port-buffer-cur! buf (1+ pos))
               (when (and (eq? u8 (char->integer #\return))
                          (eq? (peek-u8 port) (char->integer #\newline)))
                 (read-u8 port))
               (finish))
              (else
               (scan-for-newline (1+ pos))))))
          ((< cur pos)
           (accumulate-bytes! pos)
           (%set-port-buffer-cur! buf pos)
           (read-some (%fill-input port buf 1)))
          ((not has-eof?)
           (read-some (%fill-input port buf 1)))
          ((null? bytes)
           (%set-port-buffer-has-eof?! buf #f)
           (eof-object))
          (else
           (finish))))))))

(define* (write-u8 u8 #:optional (port (current-output-port)))
  (match (%port-write-buffer port)
    (#f (error "not an output port"))
    ((and buf #(dst cur end))
     (when (and (eq? cur end) (%port-r/w-random-access? port))
       (flush-input-port port))
     (cond
      ((= end (bytevector-length dst))
       ;; Multiple threads racing; race to flush, then retry.
       (flush-output-port port)
       (write-u8 u8 port))
      (else
       (bytevector-u8-set! dst end u8)
       (let ((end (1+ end)))
         (%set-port-buffer-end! buf end)
         (when (= end (bytevector-length dst))
           (flush-output-port port))))))))

(define* (write-bytevector bv #:optional (port (current-output-port))
                           (start 0) (end (bytevector-length bv)))
  (let ((count (- end start)))
    (match (%port-write-buffer port)
      (#f (error "not an output port"))
      ((and buf #(dst cur end))
       (when (and (eq? cur end) (%port-r/w-random-access? port))
         (flush-input-port port))
       (let ((size (bytevector-length dst))
             (buffered (- end cur)))
         (cond
          ((<= (+ end count) size)
           ;; Bytes fit in buffer: copy directly.
           (bytevector-copy! dst end bv start (+ start count))
           (let ((end (+ end count)))
             (%set-port-buffer-end! buf end)
             (when (= end size)
               (flush-output-port port))))
          ((< count size)
           ;; Bytes fit in buffer, but we have to flush output first.
           (flush-output-port port)
           (bytevector-copy! dst 0 bv start (+ start count))
           (%set-port-buffer-cur! buf 0)
           (%set-port-buffer-end! buf count)
           (when (= count size)
             (flush-output-port port)))
          (else
           ;; Otherwise flush any buffered output, then make an
           ;; unbuffered write.
           (unless (zero? buffered) (flush-output-port port))
           (%write-bytes port bv start count))))))))

(define* (write-char x #:optional (port (current-output-port)))
  ;; FIXME: update port position.
  (define (low-six i) (logand i #b111111))
  (let ((i (char->integer x)))
    (cond
     ((<= i #x7f)
      (write-u8 i port))
     ((<= i #x7ff)
      (write-bytevector
       (bytevector (logior #b11000000 (ash i -6))
                   (logior #b10000000 (low-six i)))
       port))
     ((<= i #xffff)
      (write-bytevector
       (bytevector (logior #b11100000 (ash i -12))
                   (logior #b10000000 (low-six (ash i -6)))
                   (logior #b10000000 (low-six i)))
       port))
     (else
      (write-bytevector
       (bytevector (logior #b11110000 (ash i -18))
                   (logior #b10000000 (low-six (ash i -12)))
                   (logior #b10000000 (low-six (ash i -6)))
                   (logior #b10000000 (low-six i)))
       port)))))

(define* (newline #:optional (port (current-output-port)))
  (write-char #\newline port))

(define* (write-string str #:optional (port (current-output-port)))
  ;; FIXME: Could avoid the double-copy and encode directly to buffer.
  (write-bytevector (string->utf8 str) port))

;; (scheme file); perhaps drop?
(define (open-binary-input-file filename) (error "files unimplemented"))
(define (open-binary-output-file filename) (error "files unimplemented"))
(define (call-with-input-file filename proc) (error "files unimplemented"))
(define (call-with-output-file filename proc) (error "files unimplemented"))
(define (delete-file filename) (error "files unimplemented"))
(define (file-exists? filename) (error "files unimplemented"))
(define (open-input-file filename) (error "files unimplemented"))
(define (open-output-file filename) (error "files unimplemented"))
(define (with-input-from-file filename thunk)
  (let ((p (open-input-file filename)))
    (parameterize ((current-input-port p))
      (call-with-values thunk
        (lambda vals
          (close-port p)
          (apply values vals))))))
(define (with-output-to-file filename thunk)
  (let ((p (open-output-file filename)))
    (parameterize ((current-output-port p))
      (call-with-values thunk
        (lambda vals
          (close-port p)
          (apply values vals))))))

;; FIXME: these should be parameters
(define (current-input-port . x) #t)
(define (current-output-port . x) #t)
(define (current-error-port . x) #t)

(define (eq? x y) (%eq? x y))
(define (eqv? x y) (%eqv? x y))
(define (equal? x y) (error "unimplemented"))
(define (not x) (if x #f #t))

(define (boolean=? . x) (error "unimplemented"))

;; R7RS strings
(define (string? x) (%string? x))
(define (string-length x) (%string-length x))
(define (string-ref x i) (%string-ref x i))
(define (string-set! x i v) (error "unimplemented"))
(define (string . chars) (list->string chars))
(define* (make-string n #:optional (init #\space))
  (error "unimplemented"))
(define (string-append . strs) (error "unimplemented"))
(define* (string-copy str #:optional (start 0) (end (string-length str)))
  (error "unimplemented"))
(define* (string-copy! to at from #:optional (start 0) (end (string-length from)))
  (error "unimplemented"))
(define (string-fill! . _) (error "unimplemented"))
(define (string-for-each f str . strs) (error "unimplemented"))
(define (string-<? x y . strs) (error "unimplemented"))
(define (string-<=? x y . strs) (error "unimplemented"))
(define (string-=? x y . strs) (error "unimplemented"))
(define (string->=? x y . strs) (error "unimplemented"))
(define (string->? x y . strs) (error "unimplemented"))
(define (string-map f str . strs) (error "unimplemented"))
(define (substring str start end) (string-copy str start end))
(define (list->string chars) (error "unimplemented"))
(define* (string->list str #:optional (start 0) (end (string-length str)))
  (let lp ((i start))
    (if (< i end)
        (cons (string-ref str i) (lp (1+ i)))
        '())))
(define* (string->vector str #:optional (start 0) (end (string-length string)))
  (list->vector (string->list str start end)))
(define* (vector->string v #:optional (start 0) (end (vector-length v)))
  (list->string (vector->list v start end)))
(define (string->utf8 str)
  (%inline-wasm
   '(func (param $str (ref eq))
          (result (ref eq))
          (local $vu0 (ref $raw-bytevector))
          (local.set $vu0
                     (array.new_default
                      $raw-bytevector
                      (string.measure_wtf8
                       (struct.get $string $str
                                   (ref.cast $string (local.get $str))))))
          (string.encode_wtf8_array
           (struct.get $string $str
                       (ref.cast $string (local.get $str)))
           (local.get $vu0)
           (i32.const 0))
          (struct.new $bytevector (i32.const 0) (local.get $vu0)))
   str))
(define* (utf8->string utf8 #:optional
                       (start 0) (end (bytevector-length utf8)))
  (unless (bytevector? utf8)
    (error "bad utf8" utf8))
  (unless (and (exact-integer? start) (<= 0 start (bytevector-length utf8)))
    (error "bad start" start))
  (unless (and (exact-integer? start) (<= start end (bytevector-length utf8)))
    (error "bad end" end))
  (%inline-wasm
   '(func (param $utf8 (ref eq))
          (param $start (ref eq))
          (param $end (ref eq))
          (result (ref eq))
          (local $i0 i32)
          (local $i1 i32)
          (local.set $i0
                     (i32.shr_s
                      (i31.get_s (ref.cast i31 (local.get $start)))
                      (i32.const 1)))
          (local.set $i1
                     (i32.shr_s
                      (i31.get_s (ref.cast i31 (local.get $end)))
                      (i32.const 1)))
          (struct.new
           $string
           (i32.const 0)
           (string.new_lossy_utf8_array
            (struct.get $bytevector $vals
                        (ref.cast $bytevector (local.get $utf8)))
            (local.get $i0)
            (i32.sub (local.get $i1) (local.get $i0)))))
   utf8 start end))

(define (symbol? x) (%symbol? x))
(define (string->symbol str) (%string->symbol str))
(define (symbol->string sym) (%symbol->string sym))
(define (symbol=? x y . z) (error "unimplemented"))

;; R7RS vectors
(define (%generic-vector . args) (list->vector args))
(define-syntax vector
  (lambda (stx)
    (syntax-case stx ()
      ((_ . x) #'(%vector . x))
      (f (identifier? #'f) #'%generic-vector))))
(define* (make-vector n #:optional init) (%make-vector n init))
(define (vector? x) (%vector? x))
(define (vector-length x) (%vector-length x))
(define (vector-ref x i) (%vector-ref x i))
(define (vector-set! x i v) (%vector-set! x i v))
(define* (vector-copy v #:optional (start 0) (end (vector-length v)))
  (error "unimplemented"))
(define* (vector-copy! to at from #:optional (start 0) (end (vector-length from)))
  (error "unimplemented"))
(define* (vector-fill! v fill #:optional (start 0) (end (vector-length v)))
  (error "unimplemented"))
(define* (vector->list v #:optional (start 0) (end (vector-length v)))
  (let lp ((i start))
    (if (< i end)
        (cons (vector-ref v i) (lp (1+ i)))
        '())))
(define (list->vector x) (error "unimplemented"))
(define (vector-append . vectors) (error "unimplemented"))
(define (vector-for-each f v . v*) (error "unimplemented"))
(define (vector-map f v . v*) (error "unimplemented"))

;; Error handling
(define (%generic-error message . args) (error "unimplemented"))
(define-syntax error
  (lambda (stx)
    (syntax-case stx ()
      ((_ msg . arg) #'(%error msg . arg))
      (f (identifier? #'f) #'%generic-error))))

(cond-expand
 (hoot-main
  (define %exception-handler (make-fluid #f))
  (define (fluid-ref* fluid depth)
    (%inline-wasm
     '(func (param $fluid (ref eq)) (param $depth (ref eq))
            (result (ref eq))
            (call $fluid-ref*
                  (ref.cast $fluid (local.get $fluid))
                  (i32.shr_s (i31.get_s (ref.cast i31 (local.get $depth)))
                             (i32.const 1))))
     fluid depth))
  ;; FIXME: Use #:key instead
  (define* (with-exception-handler handler thunk
                                   #:optional keyword (unwind? #f))
    #;
    (unless (procedure? handler)
      (error "not a procedure" handler))
    (cond
     (unwind?
      (let ((tag (make-prompt-tag "exception handler")))
        (call-with-prompt
         tag
         (lambda ()
           (with-fluids ((%exception-handler (cons #t tag)))
             (thunk)))
         (lambda (k exn)
           (handler exn)))))
     (else
      (let ((running? (make-fluid #f)))
        (with-fluids ((%exception-handler (cons running? handler)))
          (thunk))))))
  ;; FIXME: Use #:key instead
  (define* (raise-exception exn #:optional keyword continuable?)
    (let lp ((depth 0))
      ;; FIXME: fluid-ref* takes time proportional to depth, which
      ;; makes this loop quadratic.
      (match (fluid-ref* %exception-handler depth)
        (#f
         ;; No exception handlers bound; fall back.
         (pk 'uncaught-exception exn)
         (%inline-wasm
          '(func (param $exn (ref eq))
                 (call $die (string.const "uncaught exception")
                       (local.get $exn))
                 (unreachable))
          exn))
        ((#t . prompt-tag)
         (abort-to-prompt prompt-tag exn)
         (error "unreachable"))
        ((running? . handler)
         (if (fluid-ref running?)
             (begin
               (lp (1+ depth)))
             (with-fluids ((running? #t))
               (cond
                (continuable?
                 (handler exn))
                (else
                 (handler exn)
                 ;; FIXME: Raise &non-continuable.
                 (error "non-continuable")))))))))
  (%inline-wasm
   '(func (param $with-exception-handler (ref eq))
          (param $raise-exception (ref eq))
          (global.set $with-exception-handler
                      (ref.cast $proc (local.get $with-exception-handler)))
          (global.set $raise-exception
                      (ref.cast $proc (local.get $raise-exception))))
   with-exception-handler
   raise-exception))
 (hoot-aux
  ;; FIXME: #:key
  (define* (with-exception-handler handler thunk
                                   #:optional keyword (unwind? #f))
    (define with-exception-handler
      (%inline-wasm
       '(func (result (ref eq))
              (global.get $with-exception-handler))))
    (with-exception-handler handler thunk #:unwind? unwind?))
  (define* (raise-exception exn #:optional keyword continuable?)
    (%raise-exception exn #:continuable? continuable?))))

(define (raise exn) (%raise-exception exn))
(define (raise-continuable exn) (%raise-exception exn #:continuable? #t))

(define (error-object? x) (error "unimplemented"))
(define (read-error? x) (error "unimplemented"))
(define (error-object-message x) (error "unimplemented"))
(define (error-object-irritants x) (error "unimplemented"))
(define (file-error x) (error "unimplemented"))

(define (procedure? x) (%procedure? x))

;; Temp definitions!
(define (map f l)
  (let lp ((l l))
    (match l
      (() '())
      ((x . l) (cons (f x) (lp l))))))
(define (for-each f l)
  (let lp ((l l))
    (unless (null? l)
      (f (car l))
      (lp (cdr l)))))

(define (environment . import-specs) (error "eval unsupported"))
(define (interaction-environment) (error "eval unsupported"))
(define (eval exp env) (error "eval unsupported"))
(define* (load filename #:optional env) (error "load unsupported"))

(define (command-line) (error "unimplemented"))
(define (get-environment-variable name) (error "unimplemented"))
(define (get-environment-variables) (error "unimplemented"))
(define* (emergency-exit #:optional status) (error "unimplemented"))
(define* (exit #:optional status) (error "unimplemented"))

(define* (read #:optional (port (current-input-port)))
  (error "read unimplemented"))
(define* (display datum #:optional (port (current-output-port)))
  (error "display unimplemented"))
(define* (write datum #:optional (port (current-output-port)))
  (error "write unimplemented"))
(define* (write-shared datum #:optional (port (current-output-port)))
  (error "write unimplemented"))
(define* (write-simple datum #:optional (port (current-output-port)))
  (error "write unimplemented"))

(define (jiffies-per-second) (error "unimplemented"))
(define (current-jiffy) (error "unimplemented"))
(define (current-second) (error "unimplemented"))
