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

;; Guile extensions.
(define (1+ x) (%+ x 1))
(define (1- x) (%- x 1))

(define (ash x y) (%ash x y))

(define (fold f seed l)
  (let lp ((seed seed) (l l))
    (if (null? l)
        seed
        (lp (f (car l) seed)
            (cdr l)))))

(define-syntax-rule (define-associative-eta-expansion f %f)
  (begin
    (define (%generic . args)
      (let lp ((seed (%f)) (args args))
        (if (null? args)
            seed
            (lp (%f (car args) seed) (cdr args)))))
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

(define (fluid-ref x) (%fluid-ref x))
(define (fluid-set! x y) (%fluid-set! x y))
(define (with-fluid* fluid val thunk) (%with-fluid* fluid val thunk))
(define (with-dynamic-state state thunk) (%with-dynamic-state state thunk))

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
(define (make-prompt-tag . args)
  (list args))

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
    (if (null? l)
        #t
        (and (pair? l) (lp (cdr l))))))
(define (make-list n init)
  (let lp ((n n) (out '()))
    (if (zero? n)
        out
        (lp (1- n) (cons init out)))))
(define (null? x) (%null? x))

(define (reverse l)
  (let lp ((out '()) (l l))
    (if (null? l)
        out
        (lp (cons (car l) out) (cdr l)))))
;; FIXME: Multi-arg append
(define (append . args)
  (if (null? args)
      '()
      (let ((x (car args))
            (args (cdr args)))
        (if (null? args)
            x
            (let lp ((x x))
              (if (null? x)
                  (apply append args)
                  (cons (car x) (lp (cdr x)))))))))
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

(define (make-bytevector len init) (error "unimplemented"))
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
(define (bytevector . inits) (error "unimplemented"))
(define (bytevector-append . args) (error "unimplemented"))
;; FIXME: start and end args!
(define (bytevector-copy x) (error "unimplemented"))
(define (bytevector-copy! x y from) (error "unimplemented"))

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
  (if (null? y)
      x
      (let ((y (car y))
            (y* (cdr y)))
        (apply max (if (> x y) x y) y*))))
(define (min x . y)
  (if (null? y)
      x
      (let ((y (car y))
            (y* (cdr y)))
        (apply min (if (< x y) x y) y*))))

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

(define (number->string n) (error "unimplemented"))
(define (string->number n) (error "unimplemented"))

(define (rationalize x y) (error "unimplemented"))
(define (square x) (* x x))
(define (expt x y) (error "unimplemented"))
   
(define (char->integer x) (%char->integer x))
(define (integer->char x) (%integer->char x))
(define (char? x) (%char? x))
(define (char<? . args) (apply < (map char->integer args)))
(define (char<=? . args) (apply <= (map char->integer args)))
(define (char=? . args) (apply = (map char->integer args)))
(define (char>=? . args) (apply >= (map char->integer args)))
(define (char>? . args) (apply > (map char->integer args)))

;; R7RS ports
(define (char-ready? . x) (error "unimplemented"))
(define (write-char x . _) (error "unimplemented"))
(define (peek-char . x) (error "unimplemented"))
(define (read-char . x) (error "unimplemented"))
(define (close-input-port x) (error "unimplemented"))
(define (close-output-port x) (error "unimplemented"))
(define (close-port x) (error "unimplemented"))
(define (eof-object)
  (define-syntax eof-object
    (lambda (stx) #`'#,the-eof-object))
  (eof-object))
(define (eof-object? x) (%eof-object? x))
(define (get-output-string x) (error "unimplemented"))
(define (input-port? x) (error "unimplemented"))
(define (newline . x) (error "unimplemented"))
(define (open-input-string x) (error "unimplemented"))
(define (open-output-string x) (error "unimplemented"))
(define (output-port? x) (error "unimplemented"))
(define (port? x) (error "unimplemented"))
(define (u8-ready? . x) (error "unimplemented"))
(define (binary-port? x) (error "unimplemented"))
(define (textual-port? x) (error "unimplemented"))
(define (open-input-bytevector x) (error "unimplemented"))
(define (open-output-bytevector x) (error "unimplemented"))
(define (get-output-bytevector x) (error "unimplemented"))
(define (peek-u8 . x) (error "unimplemented"))
(define (read-u8 . x) (error "unimplemented"))
(define (read-bytevector . x) (error "unimplemented"))
(define (read-bytevector! dst . x) (error "unimplemented"))
(define (read-string . x) (error "unimplemented"))
(define (read-line . x) (error "unimplemented"))
(define (write-u8 . x) (error "unimplemented"))
(define (write-bytevector bv . x) (error "unimplemented"))
(define (write-string str . x) (error "unimplemented"))
(define (flush-output-port . x) (error "unimplemented"))
(define (input-port-open? x) (error "unimplemented"))
(define (output-port-open? x) (error "unimplemented"))

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
(define (string . ch) (error "unimplemented"))
(define (make-string n . ch) (error "unimplemented"))
(define (string-append . strs) (error "unimplemented"))
(define (string-copy str . start+end) (error "unimplemented"))
(define (string-copy! dst str . start+end) (error "unimplemented"))
(define (string-fill! . _) (error "unimplemented"))
(define (string-for-each f str . strs) (error "unimplemented"))
(define (string-<? x y . strs) (error "unimplemented"))
(define (string-<=? x y . strs) (error "unimplemented"))
(define (string-=? x y . strs) (error "unimplemented"))
(define (string->=? x y . strs) (error "unimplemented"))
(define (string->? x y . strs) (error "unimplemented"))
(define (string-map f str . strs) (error "unimplemented"))
(define (substring str start end) (error "unimplemented"))
(define (string->vector str) (error "unimplemented"))
(define (vector->string str) (error "unimplemented"))
(define (string->utf8 str) (error "unimplemented"))
(define (utf8->string str) (error "unimplemented"))

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
(define (make-vector n init) (%make-vector n init))
(define (vector? x) (%vector? x))
(define (vector-length x) (%vector-length x))
(define (vector-ref x i) (%vector-ref x i))
(define (vector-set! x i v) (%vector-set! x i v))
(define (vector-copy x) (error "unimplemented"))
(define (vector-copy! x) (error "unimplemented"))
(define (vector-fill! x) (error "unimplemented"))
(define (vector->list x) (error "unimplemented"))
(define (list->vector x) (error "unimplemented"))
(define (vector-append x) (error "unimplemented"))
(define (vector-for-each x) (error "unimplemented"))
(define (vector-map x) (error "unimplemented"))

;; Error handling
(define (%generic-error message . args) (error "unimplemented"))
(define-syntax error
  (lambda (stx)
    (syntax-case stx ()
      ((_ msg . arg) #'(%error msg . arg))
      (f (identifier? #'f) #'%generic-error))))

(define (error-object? x) (error "unimplemented"))
(define (raise exn) (error "unimplemented"))
(define (raise-continuable x) (error "unimplemented"))
(define (read-error? x) (error "unimplemented"))
(define (with-exception-handler x y) (error "unimplemented"))
(define (error-object-message x) (error "unimplemented"))
(define (error-object-irritants x) (error "unimplemented"))
(define (file-error x) (error "unimplemented"))

(define (procedure? x) (%procedure? x))

;; Temp definitions!
(define (map f l)
  (let lp ((l l))
    (if (null? l)
        '()
        (cons (f (car l)) (lp (cdr l))))))
(define (for-each f l)
  (let lp ((l l))
    (unless (null? l)
      (f (car l))
      (lp (cdr l)))))

;; TODO :)
(define (features) '(hoot))
