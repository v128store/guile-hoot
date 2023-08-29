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

(define (call-with-values producer consumer)
  (%call-with-values producer consumer))

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
(define (bytevector-append . args) (error "unimplemented"))
;; FIXME: start and end args!
(define* (bytevector-copy x #:optional (start 0) (end (bytevector-length x)))
  (error "unimplemented"))
(define* (bytevector-copy! to at from #:optional
                           (start 0) (end (bytevector-length from)))
  (error "unimplemented"))

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

(define* (flush-output-port #:optional (port (current-output-port)))
  (error "unimplemented"))

(define* (char-ready? #:optional (port (current-input-port)))
  (error "unimplemented"))
(define* (peek-char #:optional (port (current-input-port)))
   (error "unimplemented"))
(define* (read-char #:optional (port (current-input-port)))
  (error "unimplemented"))
(define* (u8-ready? #:optional (port (current-input-port)))
  (error "unimplemented"))
(define* (peek-u8 #:optional (port (current-input-port)))
  (error "unimplemented"))
(define* (read-u8 #:optional (port (current-input-port)))
  (error "unimplemented"))
(define* (read-bytevector k #:optional (port (current-input-port)))
  (error "unimplemented"))
(define* (read-bytevector! dst #:optional (port (current-input-port))
                           (start 0) (end (bytevector-length dst)))
  (error "unimplemented"))
(define* (read-string k #:optional (port (current-input-port)))
  (error "unimplemented"))
(define* (read-line #:optional (port (current-input-port)))
  (error "unimplemented"))

(define* (write-char x #:optional (port (current-output-port)))
  (error "unimplemented"))
(define* (newline #:optional (port (current-output-port)))
  (write-char #\newline port))
(define* (write-u8 u8 #:optional (port (current-output-port)))
  (error "unimplemented"))
(define* (write-bytevector bv #:optional (port (current-output-port)))
  (error "unimplemented"))
(define* (write-string str #:optional (port (current-output-port)))
  (error "unimplemented"))

(define (open-input-string str)
  (error "unimplemented"))

(define (open-output-string) (error "unimplemented"))
(define (get-output-string x) (error "unimplemented"))

(define (open-input-bytevector bv)
  (error "unimplemented"))

(define (open-output-bytevector) (error "unimplemented"))
(define (get-output-bytevector x) (error "unimplemented"))

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
(define (make-parameter . x) (error "unimplemented"))

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
  (let ((p (open-output-bytevector)))
    (write-string str p)
    (get-output-bytevector p)))
(define (utf8->string utf8)
  (let ((p (open-input-bytevector utf8)))
    (read-string (bytevector-length utf8) p)
    (get-output-bytevector p)))

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
