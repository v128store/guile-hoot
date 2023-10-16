;;; Hoot standard prelude
;;; Copyright (C) 2023 Igalia, S.L.
;;; Copyright (C) 2023 Robin Templeton <robin@spritely.institute>
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
(define (symbol->keyword sym) (%symbol->keyword sym))
(define (keyword->symbol kw) (%keyword->symbol kw))

(define (bitvector? x) (%bitvector? x))
(define (bitvector-length bv)
  (unless (bitvector? bv) (error "expected bitvector" bv))
  (%inline-wasm
   '(func (param $bv (ref eq))
          (result (ref eq))
          (ref.i31
           (i32.shl (struct.get $bitvector $len
                                (ref.cast $bitvector (local.get $bv)))
                    (i32.const 1))))
   bv))
(define (bitvector-ref bv i)
  (unless (bitvector? bv) (error "expected bitvector" bv))
  (unless (and (exact-integer? i) (<= 0 i) (< i (bitvector-length bv)))
    (error "index out of range" i))
  (%inline-wasm
   '(func (param $bv (ref eq))
          (param $i (ref eq))
          (result (ref eq))
          (if (ref eq)
              (i32.and
               (array.get $raw-bitvector
                          (struct.get $bitvector $vals
                                      (ref.cast $bitvector (local.get $bv)))
                          (i32.shr_s (i31.get_s (ref.cast i31 (local.get $i)))
                                     (i32.const 6)))
               (i32.shl (i32.const 1)
                        (i32.shr_s (i31.get_s (ref.cast i31 (local.get $i)))
                                   (i32.const 1))))
              (then (ref.i31 (i32.const 17)))
              (else (ref.i31 (i32.const 1)))))
   bv i))
;; bitvector-set!, list->bitvector etc not yet implemented

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
                           (then (ref.i31 (i32.const 17)))
                           (else (ref.i31 (i32.const 1)))))
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

(cond-expand
 (hoot-main
  (define default-prompt-tag (make-parameter (make-prompt-tag "%")))
  (%inline-wasm
   '(func (param $default-prompt-tag (ref eq))
          (global.set $default-prompt-tag (local.get $default-prompt-tag)))
   default-prompt-tag))
 (hoot-aux
  (define default-prompt-tag
    (%inline-wasm
     '(func (result (ref eq)) (global.get $default-prompt-tag))))))

(define (%backtrace)
  (define (scm-sp)
    (%inline-wasm
     '(func (result (ref eq))
            (ref.i31 (i32.shl (global.get $scm-sp) (i32.const 1))))))
  (define (raw-sp)
    (%inline-wasm
     '(func (result (ref eq))
            (ref.i31 (i32.shl (global.get $raw-sp) (i32.const 1))))))
  (define (ret-sp)
    (%inline-wasm
     '(func (result (ref eq))
            (ref.i31 (i32.shl (global.get $ret-sp) (i32.const 1))))))
  (define (dyn-sp)
    (%inline-wasm
     '(func (result (ref eq))
            (ref.i31 (i32.shl (global.get $dyn-sp) (i32.const 1))))))
  (define (scm-ref n)
    (%inline-wasm
     '(func (param $n (ref eq))
            (result (ref eq))
            (ref.as_non_null
             (table.get $scm-stack
                        (i32.shr_s (i31.get_s (ref.cast i31 (local.get $n)))
                                   (i32.const 1)))))
     n))
  (define (raw-ref n)
    (%inline-wasm
     '(func (param $n (ref eq))
            (result (ref eq))
            (ref.i31
             (i32.shl
              (i32.load8_s $raw-stack
                           (i32.shr_s (i31.get_s (ref.cast i31 (local.get $n)))
                                      (i32.const 1)))
              (i32.const 1))))
     n))
  (let ((scm-sp (scm-sp))
        (raw-sp (raw-sp))
        (ret-sp (ret-sp))
        (dyn-sp (dyn-sp)))
    (%debug "scm backtrace" scm-sp)
    (let lp ((i 1))
      (when (<= 0 (- scm-sp i))
        (%debug "scm" (scm-ref (- scm-sp i)))
        (lp (1+ i))))
    (%debug "raw backtrace" raw-sp)
    (let lp ((i 1))
      (when (<= 0 (- raw-sp i))
        (%debug "raw" (raw-ref (- raw-sp i)))
        (lp (1+ i))))
    (%debug "ret stack height" ret-sp)
    (%debug "dyn stack height" dyn-sp)
    (%debug "")))

;; This is an implementation of call/cc in terms of delimited
;; continuations.  It correct except as regards dynamic-wind: capturing
;; the continuation unwinds all dynamic-winds, then rewinds them; and
;; invoking the continuation does the same, even if the invoking and
;; captured continuations overlap.  Oh well; call/cc is strictly less
;; useful than call-with-prompt anyway.
(define (call-with-current-continuation proc)
  (define (unwind-and-call handler)
    (abort-to-prompt (default-prompt-tag) handler))

  (define (rewind-and-continue captured-continuation)
    (define-syntax-rule (reinstate expr)
      (captured-continuation (lambda () expr)))
    (define (k . args)
      (define (rewind-and-return-values discarded-continuation)
        (reinstate (apply values args)))
      (unwind-and-call rewind-and-return-values))
    (reinstate (proc k)))

  (let ((thunk (unwind-and-call rewind-and-continue)))
    (thunk)))

(define call/cc call-with-current-continuation)

(define-syntax %
  (syntax-rules ()
    ((_ expr)
     (call-with-prompt (default-prompt-tag)
                       (lambda () expr)
                       default-prompt-handler))
    ((_ expr handler)
     (call-with-prompt (default-prompt-tag)
                       (lambda () expr)
                       handler))
    ((_ tag expr handler)
     (call-with-prompt tag
                       (lambda () expr)
                       handler))))

(define (default-prompt-handler k proc) (% (proc k)))

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
(define (exact x)
  (cond
   ((and (real? x)
         (inexact? x)
         (finite? x))
    (%inline-wasm
     '(func (param $x (ref eq))
            (result (ref eq))
            (call $f64->exact (struct.get $flonum $val
                                          (ref.cast $flonum
                                                    (local.get $x)))))
     x))
   ((exact? x) x)
   ;; complex numbers are always inexact
   ((number? x) (error "cannot convert argument to exact number"))
   (else (error "non-numeric argument"))))

(define (quotient x y) (%quotient x y))
(define (remainder x y) (%remainder x y))
(define (modulo x y) (%modulo x y))

(define (even? x) (zero? (logand x 1)))
(define (odd? x) (not (even? x)))

(define (numerator x)
  (if (number? x)
      (cond
       ((exact-integer? x) x)
       ((exact? x)
        (%inline-wasm
         '(func (param $x (ref eq))
                (result (ref eq))
                (struct.get $fraction $num
                            (ref.cast $fraction (local.get $x))))
         x))
       (else (inexact (numerator (exact x)))))
      (error "non-numeric argument")))
(define (denominator x)
  (if (number? x)
      (cond
       ((exact-integer? x) 1)
       ((exact? x)
        (%inline-wasm
         '(func (param $x (ref eq))
                (result (ref eq))
                (struct.get $fraction $denom
                            (ref.cast $fraction (local.get $x))))
         x))
       (else (inexact (denominator (exact x)))))
      (error "non-numeric argument")))

(define (exact-integer-sqrt x)
  (unless (and (exact-integer? x) (>= x 0))
    (error "expected non-negative exact integer" x))
  (let loop ((a 1)
             (b x))
    (if (>= b a)
        (let ((m (quotient (+ a b) 2)))
          (if (< x (square m))
              (loop a (1- m))
              (loop (1+ m) b)))
        (values (1- a) (- x (square (1- a)))))))

(define (floor/ x y)
  (values (floor-quotient x y) (floor-remainder x y)))
;; Adapted from the SRFI-141 reference implementation
(define (floor-quotient x y)
  (unless (exact-integer? x)
    (error "expected exact integer" x))
  (unless (exact-integer? y)
    (error "expected exact integer" y))
  (cond
   ((and (negative? x) (negative? y))
    (quotient (- x) (- y)))
   ((negative? x)
    (let ((x (- x)))
      (call-with-values (lambda () (truncate/ x y))
        (lambda (q r)
          (if (zero? r)
              (- q)
              (1- (- q)))))))
   ((negative? y)
    (let ((y (- y)))
      (call-with-values (lambda () (truncate/ x y))
        (lambda (q r)
          (if (zero? r)
              (- q)
              (1- (- q)))))))
   (else (quotient x y))))
(define (floor-remainder x y) (modulo x y))

(define (truncate/ x y)
  (values (truncate-quotient x y)
          (truncate-remainder x y)))
(define (truncate-quotient x y) (quotient x y))
(define (truncate-remainder x y) (remainder x y))

(define (%binary-gcd x y)
  (unless (integer? x)
    (error "expected integer" x))
  (unless (integer? y)
    (error "expected integer" y))
  (let ((result
         (%inline-wasm
          '(func (param $x (ref eq)) (param $y (ref eq))
                 (result (ref eq))
                 (call $gcd (local.get $x) (local.get $y)))
          (exact x)
          (exact y))))
    (if (or (inexact x) (inexact y))
        (inexact result)
        (result))))
(define-syntax %gcd
  (syntax-rules ()
    ((_) 0)
    ((_ x) x)
    ((_ x y) (%binary-gcd x y))))

(define (%binary-lcm x y)
  (unless (integer? x)
    (error "expected integer" x))
  (unless (integer? y)
    (error "expected integer" y))
  (let* ((exact-x (exact x))
         (exact-y (exact y))
         (result (if (and (eqv? x 0) (eqv? y 0))
                     0
                     (quotient (abs (* x y)) (gcd x y)))))
    (if (or (inexact? x) (inexact? y))
        (inexact result)
        result)))
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

(define* (log x #:optional y)
  (define (%log x)
    (%inline-wasm
     '(func (param $x (ref eq)) (result (ref eq))
            (call $log (local.get $x)))
     x))
  (if y
      (/ (%log x)
         (%log y))
      (%log x)))

(define (exp x)
  (define (%exp x)
    (%inline-wasm
     '(func (param $x (ref eq)) (result (ref eq))
            (call $exp (local.get $x)))
     x))
  (%exp x))

(define* (number->string n #:optional (radix 10))
  (cond
   ((exact-integer? n)
    (if (zero? n)
        "0"
        (let* ((mag (if (< n 0) (- n) n))
               (digits
                (case radix
                  ((2) (let lp ((mag mag) (out '()))
                         (if (zero? mag)
                             out
                             (lp (ash mag -1)
                                 (cons (integer->char
                                        (+ (char->integer #\0)
                                           (logand mag 1)))
                                       out)))))
                  ((8) (let lp ((mag mag) (out '()))
                         (if (zero? mag)
                             out
                             (lp (ash mag -3)
                                 (cons (integer->char
                                        (+ (char->integer #\0)
                                           (logand mag 7)))
                                       out)))))
                  ((10) (let lp ((mag mag) (out '()))
                          (if (zero? mag)
                              out
                              (lp (quotient mag 10)
                                  (cons (integer->char
                                         (+ (char->integer #\0)
                                            (remainder mag 10)))
                                        out)))))
                  ((16) (let lp ((mag mag) (out '()))
                          (if (zero? mag)
                              out
                              (lp (ash mag -4)
                                  (cons (integer->char
                                         (let ((digit (logand mag 15)))
                                           (+ (if (< digit 10)
                                                  (char->integer #\0)
                                                  (char->integer #\a))
                                              digit)))
                                        out))))))))
          (list->string (if (negative? n) (cons #\- digits) digits)))))
   ((exact? n)
    (string-append (number->string (numerator n) radix)
                   "/"
                   (number->string (denominator n) radix)))
   ((real? n)
    (unless (eqv? radix 10)
      (error "expected radix of 10 for number->string on flonum" n))
    (%inline-wasm
     '(func (param $n (ref eq))
            (result (ref eq))
            (struct.new $string
                        (i32.const 0)
                        (call $flonum->string
                              (struct.get $flonum $val
                                          (ref.cast $flonum (local.get
                                                             $n))))))
     n))
   (else
    (string-append (number->string (real-part n) radix)
                   "/"
                   (number->string (imag-part n) radix)
                   "i"))))
(define (string->number n) (error "unimplemented"))

;; Adapted from the comments for scm_rationalize in libguile's numbers.c
(define (rationalize x y)
  (unless (rational? x)
    (error "expected rational" x))
  (unless (rational? y)
    (error "expected rational" y))
  (define (exact-rationalize x eps)
    (let ((n1  (if (negative? x) -1 1))
          (x   (abs x))
          (eps (abs eps)))
      (let ((lo (- x eps))
            (hi (+ x eps)))
        (if (<= lo 0)
            0
            (let loop ((nlo (numerator lo)) (dlo (denominator lo))
                       (nhi (numerator hi)) (dhi (denominator hi))
                       (n1 n1) (d1 0) (n2 0) (d2 1))
              (let-values (((qlo rlo) (floor/ nlo dlo))
                           ((qhi rhi) (floor/ nhi dhi)))
                (let ((n0 (+ n2 (* n1 qlo)))
                      (d0 (+ d2 (* d1 qlo))))
                  (cond ((zero? rlo) (/ n0 d0))
                        ((< qlo qhi) (/ (+ n0 n1) (+ d0 d1)))
                        (else (loop dhi rhi dlo rlo n0 d0 n1 d1))))))))))
  (if (and (exact? x) (exact? y))
      (exact-rationalize x y)
      (inexact (exact-rationalize (exact x) (exact y)))))

(define (square x) (* x x))

(define (expt x y)
  (unless (number? x)
    (error "not a number" x))
  (unless (number? y)
    (error "not a number" y))
  (cond
   ((eqv? x 0)
    (cond ((zero? y) (if (exact? y) 1 1.0))
          ((positive? y) (if (exact? y) 0 0.0))
          (else +nan.0)))
   ((eqv? x 0.0)
    (cond ((zero? y) 1.0)
          ((positive? y) 0.0)
          (else +nan.0)))
   ((exact-integer? y)
    (if (< y 0)
        (/ 1 (expt x (abs y)))
        (let lp ((y y)
                 (result 1))
          (if (= y 0)
              result
              (lp (1- y) (* x result))))))
   (else (exp (* y (log x))))))

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
(define (string-upcase str)
  (unless (string? str) (error "expected a string" str))
  (%inline-wasm
   '(func (param $str (ref eq))
          (result (ref eq))
          (struct.new
           $string
           (i32.const 0)
           (call $string-upcase
                 (struct.get $string $str
                             (ref.cast $string (local.get $str))))))
   str))
(define (string-downcase str)
  (unless (string? str) (error "expected a string" str))
  (%inline-wasm
   '(func (param $str (ref eq))
          (result (ref eq))
          (struct.new
           $string
           (i32.const 0)
           (call $string-downcase
                 (struct.get $string $str
                             (ref.cast $string (local.get $str))))))
   str))
(define (string-foldcase str)
  (string-downcase (string-upcase str)))

;; FIXME: We could use Intl.Collator instead of manually folding case.
(define (string-ci<?  . strs) (apply string-<?  (map string-foldcase strs)))
(define (string-ci<=? . strs) (apply string-<=? (map string-foldcase strs)))
(define (string-ci=?  . strs) (apply string-=?  (map string-foldcase strs)))
(define (string-ci>=? . strs) (apply string->=? (map string-foldcase strs)))
(define (string-ci>?  . strs) (apply string->?  (map string-foldcase strs)))

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
                        (ref.i31 (i32.const 17))
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
                                    (ref.i31 (i32.const 0))
                                    (ref.i31 (i32.const 0)))
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

(%define-simple-port-getter %port-open? $open?)
(%define-simple-port-getter %port-read $read)
(%define-simple-port-getter %port-write $write)
(%define-simple-port-getter %port-input-waiting? $input-waiting?)
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

(%define-simple-port-setter %set-port-open?! $open?)
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

;; FIXME: kwargs
(define (%make-soft-port repr %read-string %write-string input-waiting? close)
  (define (make-reader read-string)
    (define transcoder (open-output-bytevector))
    (define buffer #f)
    (define buffer-pos 0)
    (lambda (bv start count)
      (unless (and buffer (< buffer-pos (bytevector-length buffer)))
        (let* ((str (%read-string)))
          (write-string str transcoder)
          (set! buffer (get-output-bytevector transcoder #t))
          (set! buffer-pos 0)))

      (let* ((to-copy (min count (- (bytevector-length buffer) buffer-pos)))
             (next-pos (+ buffer-pos to-copy)))
        (bytevector-copy! bv start buffer buffer-pos next-pos)
        (if (= (bytevector-length buffer) next-pos)
            (set! buffer #f)
            (set! buffer-pos next-pos))
        to-copy)))

  (define (make-writer write-string)
    (lambda (bv start count)
      ;; FIXME: If the writer is binary, that could split a codepoint in
      ;; two, resulting in badness.  Shouldn't happen with textual
      ;; writers but it's worth noting.
      (%write-string (utf8->string bv start (+ start count)))
      count))

  (unless (string? repr)
    (error "invalid repr" repr))
  (define default-buffer-size 1024)
  (%make-port (and read-string (make-reader read-string))
              (and write-string (make-writer write-string))
              input-waiting?
              #f                        ; seek
              #f                        ; close
              #f                        ; truncate
              repr                      ; repr
              #f                        ; file-name
              default-buffer-size       ; read-buf-size
              default-buffer-size       ; write-buf-size
              #f                        ; r/w-random-access
              #f                        ; private data
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

(define (port? x)
  (%inline-wasm '(func (param $obj (ref eq))
                       (result (ref eq))
                       (if (ref eq)
                           (ref.test $port (local.get $obj))
                           (then (ref.i31 (i32.const 17)))
                           (else (ref.i31 (i32.const 1)))))
                x))
(define (input-port? x) (and (port? x) (%port-read x) #t))
(define (output-port? x) (and (port? x) (%port-write x) #t))
(define (binary-port? x) (port? x))
(define (textual-port? x) (port? x))
(define (input-port-open? x)
  (unless (input-port? x) (error "not an input port"))
  (%port-open? x))
(define (output-port-open? x)
  (unless (output-port? x) (error "not an output port"))
  (%port-open? x))
(define (close-input-port x)
  (unless (input-port? x) (error "not an input port"))
  ;; FIXME: Allow half-closing of socket-like ports.
  (close-port x))
(define (close-output-port x)
  (unless (output-port? x) (error "not an output port"))
  ;; FIXME: Allow half-closing of socket-like ports.
  (close-port x))
(define (close-port port)
  (unless (port? port) (error "not a port" port))
  (when (%port-open? port)
    (when (output-port? port) (flush-output-port port))
    (%set-port-open?! port #f))
  (values))

(define (seek port offset whence)
  (unless (port? port) (error "not a port" port))
  (unless (exact-integer? offset) (error "bad offset" offset))
  (case whence ((cur start end) #t) (else (error "bad whence" whence)))
  (define (buffered-bytes buf)
    (define (port-buffer-cur buf) (vector-ref buf 1))
    (define (port-buffer-end buf) (vector-ref buf 2))
    (if (vector? buf)
        (- (port-buffer-end buf) (port-buffer-cur buf))
        0))
  (cond
   ((%port-seek port)
    => (lambda (%seek)
         (cond
          ((and (eq? whence 'cur) (zero? offset))
           ;; Query current position, adjust for buffering without
           ;; flush.
           (let ((pos (%seek offset whence))
                 (buf-in (buffered-bytes (%port-read-buffer port)))
                 (buf-out (buffered-bytes (%port-write-buffer port))))
             (+ pos (- buf-in) buf-out)))
          ((not (%port-r/w-random-access? port))
           (error "port is not seekable"))
          (else
           (when (input-port? port) (flush-input-port port))
           (when (output-port? port) (flush-output-port port))
           (let ((pos (%seek offset whence)))
             (when (input-port? port)
               (%set-port-buffer-has-eof?! (%port-read-buffer port) #f))
             pos)))))
   (else (error "port is not seekable" port))))

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
    (#f (error "not an input port"))
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
  (match (%port-read-buffer port)
    (#f (error "not an input port"))
    (#(bv cur end has-eof?)
     (or (< cur end)
         has-eof?
         (match (%port-input-waiting? port)
           (#f #t)
           (proc (proc)))))))

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
  (u8-ready? port))

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
                       (ref.i31
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
                       (ref.i31
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

(define (eq? x y) (%eq? x y))
(define (eqv? x y) (%eqv? x y))

(define (equal? x y)
  (cond
   ((eqv? x y) #t)
   ((pair? x)
    (and (pair? y)
         (equal? (car x) (car y))
         (equal? (cdr x) (cdr y))))
   ((vector? x)
    (and (vector? y)
         (let ((length (vector-length x)))
           (and (= length (vector-length y))
                (let lp ((i 0))
                  (if (= i length)
                      #t
                      (and (equal? (vector-ref x i) (vector-ref y i))
                           (lp (+ i 1)))))))))
   ((string? x)
    (and (string? y)
         (string-=? x y)))
   ((bytevector? x)
    (and (bytevector? y)
         (let ((length (bytevector-length x)))
           (and (= length (bytevector-length y))
                (let lp ((i 0))
                  (if (= i length)
                      #t
                      (and (eqv? (bytevector-u8-ref x i)
                                 (bytevector-u8-ref y i))
                           (lp (+ i 1)))))))))
   ((bitvector? x)
    (and (bitvector? y)
         (let ((length (bitvector-length x)))
           (and (= length (bitvector-length y))
                (let lp ((i 0))
                  (if (= i length)
                      #t
                      (and (eqv? (bitvector-ref x i)
                                 (bitvector-ref y i))
                           (lp (+ i 1)))))))))
   (else #f)))

(define (not x) (if x #f #t))

(define (and-map pred l)
  (or (null? l)
      (and (pred (car l))
           (and-map pred (cdr l)))))

(define (boolean? x) (match x ((or #f #t) #t) (_ #f)))
(define (boolean=? x y . z)
  (unless (and (boolean? x) (boolean? y) (and-map boolean? z))
    (error "expected booleans" x y z))
  (apply eq? x y z))

;; R7RS strings
(define (string? x) (%string? x))
(define (string-length x) (%string-length x))
(define (string-ref x i) (%string-ref x i))
(define (string-set! x i v)
  (error "string-set!: mutable strings unimplemented"))
(define (string . chars) (list->string chars))
(define* (make-string n #:optional (init #\space))
  (let ((p (open-output-string)))
    (let lp ((n n))
      (unless (zero? n)
        (write-char init p)
        (lp (1- n))))
    (get-output-string p)))
(define (string-append . strs)
  (let ((p (open-output-string)))
    (for-each (lambda (str) (write-string str p)) strs)
    (get-output-string p)))
(define* (string-copy str #:optional (start 0) (end (string-length str)))
  (unless (string? str) (error "expected string" str))
  (unless (and (exact-integer? start) (<= 0 start (string-length str)))
    (error "bad start" start))
  (unless (and (exact-integer? end) (<= start end (string-length str)))
    (error "bad end" end))
  (%inline-wasm
   '(func (param $str (ref eq))
          (param $start (ref eq))
          (param $end (ref eq))
          (result (ref eq))
          (local $i0 i32)
          (local $i1 i32)
          (local $str_iter (ref stringview_iter))
          (local.set $i0
                     (i32.shr_s (i31.get_s (ref.cast i31 (local.get $start)))
                                (i32.const 1)))
          (local.set $i1
                     (i32.shr_s (i31.get_s (ref.cast i31 (local.get $end)))
                                (i32.const 1)))
          (local.set $str_iter
                     (string.as_iter
                      (struct.get $string $str
                                  (ref.cast $string (local.get $str)))))
          (drop
           (stringview_iter.advance (local.get $str_iter) (local.get $i0)))
          (struct.new $string
                      (i32.const 0)
                      (stringview_iter.slice (local.get $str_iter)
                                             (i32.sub (local.get $i1)
                                                      (local.get $i0)))))
   str start end))
(define* (string-copy! to at from #:optional (start 0) (end (string-length from)))
  (error "string-copy!: mutable strings unimplemented"))
(define (string-fill! . _)
  (error "string-fill!: mutable strings unimplemented"))
(define (string-for-each f str . strs)
  (unless (and (string? str) (and-map string? strs))
    (error "expected strings" str strs))
  (apply for-each f (string->list str) (map string->list strs)))
(define (%string-compare a b)
  (if (eq? a b)
      0
      (%inline-wasm
       '(func (param $a (ref eq))
              (param $b (ref eq))
              (result (ref eq))
              (ref.i31
               (i32.shl
                (string.compare
                 (struct.get $string $str (ref.cast $string (local.get $a)))
                 (struct.get $string $str (ref.cast $string (local.get $b))))
                (i32.const 1))))
       a b)))
(define (%string-compare* ordered? x y strs)
  (unless (and (string? x) (string? y) (and-map string? strs))
    (error "expected strings" x y strs))
  (define (pred a b) (ordered? (%string-compare a b) 0))
  (and (pred x y)
       (let lp ((y y) (strs strs))
         (match strs
           (() #t)
           ((z . strs) (and (pred y z) (lp z strs)))))))
(define (string-<?  x y . strs) (%string-compare* <  x y strs))
(define (string-<=? x y . strs) (%string-compare* <= x y strs))
(define (string-=?  x y . strs) (%string-compare* =  x y strs))
(define (string->=? x y . strs) (%string-compare* >= x y strs))
(define (string->?  x y . strs) (%string-compare* >  x y strs))
(define (substring str start end) (string-copy str start end))
(define (list->string chars)
  (let ((p (open-output-string)))
    (for-each (lambda (ch) (write-char ch p)) chars)
    (get-output-string p)))
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
(define (string->symbol str)
  (unless (string? str) (error "expected string" str))
  (%string->symbol str))
(define (symbol->string sym)
  (unless (symbol? sym) (error "expected symbol" sym))
  (%symbol->string sym))
(define (symbol=? x y . z)
  (unless (and (symbol? x) (symbol? y) (and-map symbol? z))
    (error "expected symbols" x y z))
  (apply eq? x y z))

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
  (unless (vector? v) (error "expected vector" v))
  (unless (and (exact-integer? start) (<= 0 start (vector-length v)))
    (error "bad start" start))
  (unless (and (exact-integer? end) (<= start end (vector-length v)))
    (error "bad end" end))
  (%inline-wasm
   '(func (param $src (ref eq)) (param $start (ref eq)) (param $end (ref eq))
          (result (ref eq))
          (local $i0 i32)
          (local $i1 i32)
          (local $v0 (ref $raw-scmvector))
          (local.set $i0 (i32.shr_u
                          (i31.get_u (ref.cast i31 (local.get $start)))
                          (i32.const 1)))
          (local.set $i1 (i32.sub
                          (i32.shr_u
                           (i31.get_u (ref.cast i31 (local.get $end)))
                           (i32.const 1))
                          (local.get $i0)))
          (local.set $v0 (array.new $raw-scmvector (ref.i31 (i32.const 0))
                                    (local.get $i1)))
          (array.copy $raw-scmvector $raw-scmvector
                      (local.get $v0) (i32.const 0)
                      (struct.get $vector $vals
                                  (ref.cast $vector (local.get $src)))
                      (local.get $i0) (local.get $i1))
          (struct.new $mutable-vector (i32.const 0) (local.get $v0)))
   v start end))
(define* (vector-copy! to at from #:optional (start 0) (end (vector-length from)))
  (unless (vector? to) (error "expected mutable vector" to))
  (unless (and (exact-integer? at) (<= 0 at (vector-length to)))
    (error "bad at" at))
  (unless (vector? from) (error "expected vector" from))
  (unless (and (exact-integer? start) (<= 0 start (vector-length from)))
    (error "bad start" start))
  (unless (and (exact-integer? end) (<= start end (vector-length from)))
    (error "bad end" end))
  (%inline-wasm
   '(func (param $to (ref eq)) (param $at (ref eq))
          (param $from (ref eq)) (param $start (ref eq)) (param $end (ref eq))
          (array.copy $raw-scmvector $raw-scmvector
                      (struct.get $mutable-vector $vals
                                  (ref.cast $mutable-vector (local.get $to)))
                      (i32.shr_u
                       (i31.get_u (ref.cast i31 (local.get $at)))
                       (i32.const 1))
                      (struct.get $vector $vals
                                  (ref.cast $vector (local.get $from)))
                      (i32.shr_u
                       (i31.get_u (ref.cast i31 (local.get $start)))
                       (i32.const 1))
                      (i32.shr_u
                       (i32.sub
                        (i31.get_u (ref.cast i31 (local.get $end)))
                        (i31.get_u (ref.cast i31 (local.get $start))))
                       (i32.const 1))))
   to at from start end))
(define* (vector-fill! v fill #:optional (start 0) (end (vector-length v)))
  (unless (vector? v) (error "expected vector" v))
  (unless (and (exact-integer? start) (<= 0 start (vector-length v)))
    (error "bad start" start))
  (unless (and (exact-integer? end) (<= start end (vector-length v)))
    (error "bad end" end))
  (%inline-wasm
   '(func (param $dst (ref eq)) (param $fill (ref eq))
          (param $start (ref eq)) (param $end (ref eq))
          (local $i0 i32)
          (local $i1 i32)
          (local.set $i0 (i32.shr_u
                          (i31.get_u (ref.cast i31 (local.get $start)))
                          (i32.const 1)))
          (local.set $i1 (i32.sub
                          (i32.shr_u
                           (i31.get_u (ref.cast i31 (local.get $end)))
                           (i32.const 1))
                          (local.get $i0)))
          ;; FIXME: Remove this debugging call.  With the current
          ;; version of V8 though, doing so causes the array.fill
          ;; instruction to go wrong; test with:
          ;;  bin/eval.scm '(let ((v (vector 1 2 3))) (vector-fill! v #t 1 2) v)'
          ;; We expect #(1 true 3) but get 65536 (!!!!)
          (call $debug-str-scm (string.const "before fill") (local.get $dst))
          (array.fill $raw-scmvector
                      (struct.get $mutable-vector $vals
                                  (ref.cast $mutable-vector (local.get $dst)))
                      (local.get $i0)
                      (local.get $fill)
                      (local.get $i1))
          )
   v fill start end))
(define* (vector->list v #:optional (start 0) (end (vector-length v)))
  (let lp ((i start))
    (if (< i end)
        (cons (vector-ref v i) (lp (1+ i)))
        '())))
(define (list->vector elts)
  (let* ((len (length elts))
         (v (make-vector len #f)))
    (let lp ((i 0) (elts elts))
      (match elts
        (() v)
        ((elt . elts)
         (vector-set! v i elt)
         (lp (1+ i) elts))))))
(define (vector-concatenate v*)
  (match v*
    (() #())
    ((v) v)
    (v*
     (let* ((len (fold (lambda (v len) (+ (vector-length v) len)) 0 v*))
            (flattened (make-vector len 0)))
       (let lp ((v* v*) (cur 0))
         (match v*
           (() flattened)
           ((v . v*)
            (vector-copy! flattened cur v)
            (lp v* (+ cur (vector-length v))))))))))
(define (vector-append . vectors)
  (vector-concatenate vectors))
(define (vector-for-each f v . v*)
  (apply for-each f (vector->list v) (map vector->list v*)))
(define (vector-map f v . v*)
  (list->vector (apply map f (vector->list v) (map vector->list v*))))

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
  (define with-exception-handler
    (%inline-wasm
     '(func (result (ref eq))
            (global.get $with-exception-handler))))
  ;; FIXME: #:key
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

(define* (%write-datum port x #:optional quote-strings?)
  (define (recur x) (%write-datum port x quote-strings?))
  (cond
   ((eq? x #f)         (write-string "#f" port))
   ((eq? x #t)         (write-string "#t" port))
   ((eq? x #nil)       (write-string "#nil" port))
   ((eq? x '())        (write-string "()" port))
   ((eq? x (if #f #f)) (write-string "#<unspecified>" port))
   ((eof-object? x)    (write-string "#<eof>" port))
   ((number? x)        (write-string (number->string x) port))
   ((char? x)
    (case x
      ((#\alarm)     (write-string "#\\alarm" port))
      ((#\backspace) (write-string "#\\backspace" port))
      ((#\delete)    (write-string "#\\delete" port))
      ((#\escape)    (write-string "#\\escape" port))
      ((#\newline)   (write-string "#\\newline" port))
      ((#\null)      (write-string "#\\null" port))
      ((#\return)    (write-string "#\\return" port))
      ((#\space)     (write-string "#\\space" port))
      ((#\tab)       (write-string "#\\tab" port))
      ((#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
        #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z
        #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
        #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z
        #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
        #\` #\~ #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\_ #\= #\+
        #\[ #\] #\{ #\} #\\ #\| #\; #\: #\' #\" #\< #\> #\, #\. #\/ #\?)
       (write-char #\# port)
       (write-char #\\ port)
       (write-char x port))
      (else
       (write-char #\# port)
       (write-char #\\ port)
       (write-char #\x port)
       (write-string (number->string (char->integer x) 16) port))))
   ((pair? x)
    (write-char #\( port)
    (recur (car x))
    (let lp ((tail (cdr x)))
      (cond
       ((null? tail)
        (write-char #\) port))
       ((pair? tail)
        (write-char #\space port)
        (recur (car tail))
        (lp (cdr tail)))
       (else
        (write-string " . " port)
        (recur tail)
        (write-char #\) port)))))
   ((string? x)
    (cond
     (quote-strings?
      (write-char #\" port)
      (string-for-each (lambda (ch)
                         (when (or (eq? ch #\\) (eq? ch #\"))
                           (write-char #\\ port))
                         (write-char ch port))
                       x)
      (write-char #\" port))
     (else
      (write-string x port))))
   ((symbol? x)
    (%write-datum port (symbol->string x) #f))
   ((vector? x)
    (write-char #\# port)
    (recur (vector->list x)))
   ((bytevector? x)
    (write-string "#vu8(" port)
    (let lp ((i 0))
      (when (< i (bytevector-length x))
        (unless (zero? i)
          (write-char #\space port))
        (write-string (number->string (bytevector-u8-ref x i)) port)
        (lp (1+ i))))
    (write-char #\) port))
   ((bitvector? x)
    (write-string "#*" port)
    (let lp ((i 0))
      (when (< i (bitvector-length x))
        (write-char (if (bitvector-ref x i) #\1 #\0) port)
        (lp (1+ i)))))
   ((procedure? x)
    (write-string "#<procedure>" port))
   ((keyword? x)
    (write-string "#:" port)
    (write-string (symbol->string (keyword->symbol x)) port))
   (else
    (recur "unhandled object :("))))

(define* (read #:optional (port (current-input-port)))
  (error "read unimplemented"))
(define* (display datum #:optional (port (current-output-port)))
  (%write-datum port datum #f))
(define* (write datum #:optional (port (current-output-port)))
  (%write-datum port datum #t))
(define* (write-shared datum #:optional (port (current-output-port)))
  (error "write unimplemented"))
(define* (write-simple datum #:optional (port (current-output-port)))
  (error "write unimplemented"))

(define (jiffies-per-second) (error "unimplemented"))
(define (current-jiffy) (error "unimplemented"))
(define (current-second) (error "unimplemented"))

(define (standard-input-port)
    (%make-soft-port "stdin"
                     (lambda ()
                       (%inline-wasm
                        '(func (result (ref eq))
                               (struct.new $string
                                           (i32.const 0)
                                           (call $read-stdin)))))
                     #f #f #f))
(define (standard-output-port)
  (%make-soft-port "stdout"
                   #f
                   (lambda (str)
                     (%inline-wasm
                      '(func (param $str (ref eq))
                             (call $write-stdout
                                   (struct.get $string $str
                                               (ref.cast $string
                                                         (local.get $str)))))
                      str))
                   #f #f))
(define (standard-error-port)
  (%make-soft-port "stderr"
                   #f
                   (lambda (str)
                     (%inline-wasm
                      '(func (param $str (ref eq))
                             (call $write-stderr
                                   (struct.get $string $str
                                               (ref.cast $string
                                                         (local.get $str)))))
                      str))
                   #f #f))

(cond-expand
 (hoot-main
  (define current-input-port
    (make-parameter (standard-input-port)
                    (lambda (val)
                      (unless (input-port? val)
                        (error "expected input port" val))
                      val)))
  (define current-output-port
    (make-parameter (standard-output-port)
                    (lambda (val)
                      (unless (output-port? val)
                        (error "expected output port" val))
                      val)))
  (define current-error-port
    (make-parameter (standard-error-port)
                    (lambda (val)
                      (unless (output-port? val)
                        (error "expected output port" val))
                      val)))
  (%inline-wasm
   '(func (param $current-input-port (ref eq))
          (param $current-output-port (ref eq))
          (param $current-error-port (ref eq))
          (global.set $current-input-port (local.get $current-input-port))
          (global.set $current-output-port (local.get $current-output-port))
          (global.set $current-error-port (local.get $current-error-port)))
   current-input-port
   current-output-port
   current-error-port))
 (hoot-aux
  (define current-input-port
    (%inline-wasm
     '(func (result (ref eq)) (global.get $current-input-port))))
  (define current-output-port
    (%inline-wasm
     '(func (result (ref eq)) (global.get $current-output-port))))
  (define current-error-port
    (%inline-wasm
     '(func (result (ref eq)) (global.get $current-error-port))))))
