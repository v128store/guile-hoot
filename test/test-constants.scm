;;; WebAssembly binary parser
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
;;; Parser for WebAssembly binary format
;;;
;;; Code:

(use-modules (wasm assemble)
             (hoot compile)
             (ice-9 binary-ports)
             (ice-9 match)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-64))

(define d8 (or (getenv "D8") "d8"))
(define srcdir (or (getenv "SRCDIR") (getcwd)))

(define (scope-file file-name)
  (string-append srcdir "/" file-name))

(test-begin "test-constants")

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
  (let* ((args (cons* "--experimental-wasm-gc"
                      "--experimental-wasm-stringref"
                      "--experimental-wasm-return-call"
                      args))
         (port (apply open-pipe* OPEN_READ d8 args))
         (output (get-string-all port)))
    (close-port port)
    (string-trim-both output)))

(define (compile-scheme-then-load-wasm-in-d8 constant)
  (call-with-compiled-wasm-file
   (compile constant)
   (lambda (wasm-file-name)
     (run-d8 (scope-file "test/load-wasm-and-print.js") "--" srcdir wasm-file-name))))

(define (compile-call form)
  (let lp ((form form) (files '()) (first? #t))
    (match form
      (()
       (apply run-d8 (scope-file "test/test-call.js") "--" srcdir (reverse files)))
      ((x . form)
       (call-with-compiled-wasm-file
        (compile x #:import-abi? (not first?) #:export-abi? first?)
        (lambda (file)
          (lp form (cons file files) #f)))))))

(define-syntax-rule (test-compilation expr repr)
  (test-equal repr repr (compile-scheme-then-load-wasm-in-d8 'expr)))
(define-syntax-rule (test-call repr proc arg ...)
  (test-equal repr repr (compile-call '(proc arg ...))))

(test-compilation 42 "42")
(test-compilation 100 "100")
(test-compilation -1 "-1")
(test-compilation -42 "-42")
(test-compilation #f "false")
(test-compilation '#nil "#nil")
(test-compilation '() "()")
(test-compilation #t "true")
(test-compilation #\a "#\\a")
(test-compilation (if #f #f) "#<unspecified>")
;(test-compilation the-eof-object "#<eof>")
(test-compilation '(1 . 2) "(1 . 2)")
(test-compilation '(1 2 3 4) "(1 2 3 4)")
(test-compilation #() "#()")
(test-compilation #(3 10 (42)) "#(3 10 (42))")
(test-compilation #vu8() "#vu8()")
(test-compilation #vu8(3 10 42) "#vu8(3 10 42)")
(test-compilation #* "#*")
(test-compilation #*101 "#*101")
(test-compilation #*100000 "#*100000")
(test-compilation #*0100000 "#*0100000")
(test-compilation "foo" "foo")
(test-compilation (lambda () 42) "#<procedure>")
(test-compilation #:foo "#:foo")
(test-compilation 'TZAG "TZAG")

(test-call "42" (lambda () 42))
(test-call "69" (lambda (x) x) 69)
(test-call "hey" (lambda (x) (if x 'hey 'ho)) #t)
(test-call "hey2" (lambda (x) (if x 'hey2 'ho)) 42)
(test-call "ho" (lambda (x) (if x 'hey3 'ho)) #f)
(test-call "10" (lambda (x y) (+ x y)) 6 4)
(test-call "rerro" (lambda () 'rerro))
(test-call "1337" (lambda (f) (f)) (lambda () 1337))
(test-call "43" (lambda (f) (+ (f) 1)) (lambda () 42))
(test-call "120" (lambda (n)
                   (let fac ((n n))
                     (if (eq? n 0)
                         1
                         (* n (fac (1- n))))))
           5)
(test-call "9227465" (lambda (n)
                       (let fib ((n n))
                         (if (<= n 1)
                             1
                             (+ (fib (- n 1)) (fib (- n 2))))))
           34)

(test-call "1000000" (lambda ()
                       (let lp ((n 0))
                         (if (< n #e1e6)
                             (lp (1+ n))
                             n))))

;;; Fixnum overflow
;; add and sub
(test-call "536870912" (lambda (a b) (+ a b)) 536870911 1)
(test-call "-536870913" (lambda (a b) (- a b)) -536870912 1)
;; add/immediate and sub/immediate
(test-call "536870912" (lambda (a) (1+ a)) 536870911)
(test-call "-536870913" (lambda (a) (1- a)) -536870912)
(test-call "536870953" (lambda (a) (+ a 42)) 536870911)
(test-call "-536870954" (lambda (a) (- a 42)) -536870912)
;; mul
(test-call "391" (lambda (a b) (* a b)) 17 23)
(test-call "536895241" (lambda (a) (* a a)) 23171)

;; Construct two bignums and add them (vs. previous tests producing
;; bignum sums from fixnum inputs)
(test-call "1073741824" (lambda (a b) (+ (+ a b) (+ a b))) 536870911 1)
(test-call "536870954" (lambda (a b c) (+ (+ a b) c)) 536870911 1 42)
(test-call "536870954" (lambda (a b c) (+ c (+ a b))) 536870911 1 42)

(test-call "0" (lambda (a b) (- (+ a b) (+ a b))) 536870911 1)
(test-call "536870870" (lambda (a b c) (- (+ a b) c)) 536870911 1 42)
(test-call "-536870870" (lambda (a b c) (- c (+ a b))) 536870911 1 42)

(test-call "1073741824" (lambda (a b c) (* (+ a b) c)) 536870911 1 2)
(test-call "1073741824" (lambda (a b c) (* c (+ a b))) 536870911 1 2)
(test-call "288230376151711744" (lambda (a b) (* (+ a b) (+ a b))) 536870911 1)

;; Flonum addition
(test-call "84" (lambda (a) (+ (inexact a) (inexact a))) 42)
(test-call "84" (lambda (a) (+ (inexact a) a)) 42)
(test-call "536870954" (lambda (a b c) (+ (inexact a) (+ b c))) 42 536870911 1)
(test-call "84" (lambda (a) (+ a (inexact a))) 42)
(test-call "536870954" (lambda (a b c) (+ (+ a b) (inexact c))) 536870911 1 42)

;; Flonum subtraction
(test-call "0" (lambda (a) (- (inexact a) (inexact a))) 42)
(test-call "0" (lambda (a) (- (inexact a) a)) 42)
(test-call "-536870870" (lambda (a b c) (- (inexact a) (+ b c))) 42 536870911 1)
(test-call "0" (lambda (a) (- a (inexact a))) 42)
(test-call "536870870" (lambda (a b c) (- (+ a b) (inexact c))) 536870911 1 42)

;; Flonum multiplication
(test-call "1764" (lambda (a) (* (inexact a) (inexact a))) 42)
(test-call "1764" (lambda (a) (* (inexact a) a)) 42)
(test-call "22548578304" (lambda (a b c) (* (inexact a) (+ b c))) 42 536870911 1)
(test-call "1764" (lambda (a) (* a (inexact a))) 42)
(test-call "22548578304" (lambda (a b c) (* (+ a b) (inexact c))) 536870911 1 42)

;; Flonum division
(test-call "5.25"
           (lambda (a b) (/ (inexact a) (inexact b)))
           42 8)
(test-call "0.5" (lambda (a b) (/ (inexact a) b)) 1 2)
(test-call "1"
           (lambda (a b) (/ (inexact (+ a b)) (+ a b)))
           536870911 1)
(test-call "0.5" (lambda (a b) (/ a (inexact b))) 1 2)
(test-call "107374182.4"
           (lambda (a b c) (/ (+ a b) (inexact c)))
           536870911 1 5)

;; Exact fraction arithmetic
(test-call "1/2" (lambda (a b) (/ a b)) 1 2)
(test-call "3/536870912" (lambda (a b c) (/ a (+ b c))) 3 536870911 1)
(test-call "536870912/3" (lambda (a b c) (/ (+ a b) c)) 536870911 1 3)
(test-call "536870912/536870913"
           (lambda (a b) (/ (+ a b) (+ a b b)))
           536870911 1)

(test-call "1/2" (lambda (a b) (* a (/ a b))) 1 2)
(test-call "1" (lambda (a b) (* b (/ a b))) 1 2)
(test-call "1/4" (lambda (a b) (* (/ a b) (/ a b))) 2 4)
(test-call "1" (lambda (a b) (* (/ b a) (/ a b))) 1 2)
;; FIXME:
;; (test-call "1"
;;            (lambda (a b) (* (/ (+ a b) (+ a b)) (/ (+ a b) (+ a b))))
;;            536870911 1)

(test-call "3/2" (lambda (a b c) (+ a (/ b c))) 1 1 2)
(test-call "1073741825/2"
           (lambda (a b c d) (+ (+ a b) (/ c d)))
           536870911 1 1 2)
(test-call "7/6" (lambda (a b c d) (+ (/ a b) (/ c d))) 1 2 2 3)
(test-call "7/6" (lambda (a b c d) (+ (/ a b) (/ c d))) 2 4 4 6)

(test-call "1/2" (lambda (a b c) (- a (/ b c))) 1 1 2)
(test-call "-1/2" (lambda (a b c) (- (/ a b) c)) 1 2 1)
(test-call "-1/2" (lambda (a b c) (- (/ a b) (/ c c))) 1 2 1)
(test-call "1610612735/3"
           (lambda (a b c d) (- (+ a b) (/ c d)))
           536870911 1 1 3)
(test-call "-1610612735/3"
           (lambda (a b c d) (- (/ a b) (+ c d)))
           1 3 536870911 1)
(test-call "-1/6" (lambda (a b c d) (- (/ a b) (/ c d))) 1 2 2 3)
(test-call "-1/2" (lambda (a b) (/ a b)) 1 -2)

;; Integer division
;; Quotient
(test-call "12" (lambda (a b) (quotient a b)) 123 10)
(test-call "-12" (lambda (a b) (quotient a b)) 123 -10)
(test-call "-12" (lambda (a b) (quotient a b)) -123 10)
(test-call "12" (lambda (a b) (quotient a b)) -123 -10)

(test-call "53687091" (lambda (a b c) (quotient (+ a b) c)) 536870911 1 10)
(test-call "-53687091" (lambda (a b c) (quotient (+ a b) c)) 536870911 1 -10)
(test-call "-53687091" (lambda (a b c) (quotient (+ a b) c)) -536870911 -1 10)
(test-call "53687091" (lambda (a b c) (quotient (+ a b) c)) -536870911 -1 -10)

(test-call "0" (lambda (a b c) (quotient a (+ b c))) 10 536870911 1)
(test-call "0" (lambda (a b c) (quotient a (+ b c))) 10 -536870911 -1)
(test-call "0" (lambda (a b c) (quotient a (+ b c))) -10 536870911 1)
(test-call "0" (lambda (a b c) (quotient a (+ b c))) -10 -536870911 -1)

(test-call "1"
           (lambda (a b c d) (quotient (+ a b) (+ c d)))
           536870911 2 536870911 1)
(test-call "-1"
           (lambda (a b c d) (quotient (+ a b) (+ c d)))
           536870911 2 -536870911 -1)
(test-call "-1"
           (lambda (a b c d) (quotient (+ a b) (+ c d)))
           -536870911 -2 536870911 1)
(test-call "1"
           (lambda (a b c d) (quotient (+ a b) (+ c d)))
           -536870911 -2 -536870911 -1)

;; Remainder
(test-call "3" (lambda (a b) (remainder a b)) 123 10)
(test-call "3" (lambda (a b) (remainder a b)) 123 -10)
(test-call "-3" (lambda (a b) (remainder a b)) -123 10)
(test-call "-3" (lambda (a b) (remainder a b)) -123 -10)

(test-call "2" (lambda (a b c) (remainder (+ a b) c)) 536870911 1 10)
(test-call "2" (lambda (a b c) (remainder (+ a b) c)) 536870911 1 -10)
(test-call "-2" (lambda (a b c) (remainder (+ a b) c)) -536870911 -1 10)
(test-call "-2" (lambda (a b c) (remainder (+ a b) c)) -536870911 -1 -10)

(test-call "10" (lambda (a b c) (remainder a (+ b c))) 10 536870911 1)
(test-call "10" (lambda (a b c) (remainder a (+ b c))) 10 -536870911 -1)
(test-call "-10" (lambda (a b c) (remainder a (+ b c))) -10 536870911 1)
(test-call "-10" (lambda (a b c) (remainder a (+ b c))) -10 -536870911 -1)

(test-call "1"
           (lambda (a b c d) (remainder (+ a b) (+ c d)))
           536870911 2 536870911 1)
(test-call "1"
           (lambda (a b c d) (remainder (+ a b) (+ c d)))
           536870911 2 -536870911 -1)
(test-call "-1"
           (lambda (a b c d) (remainder (+ a b) (+ c d)))
           -536870911 -2 536870911 1)
(test-call "-1"
           (lambda (a b c d) (remainder (+ a b) (+ c d)))
           -536870911 -2 -536870911 -1)

;; Modulo
(test-call "3" (lambda (a b) (modulo a b)) 123 10)
(test-call "-7" (lambda (a b) (modulo a b)) 123 -10)
(test-call "7" (lambda (a b) (modulo a b)) -123 10)
(test-call "-3" (lambda (a b) (modulo a b)) -123 -10)

(test-call "2" (lambda (a b c) (modulo (+ a b) c)) 536870911 1 10)
(test-call "-8" (lambda (a b c) (modulo (+ a b) c)) 536870911 1 -10)
(test-call "8" (lambda (a b c) (modulo (+ a b) c)) -536870911 -1 10)
(test-call "-2" (lambda (a b c) (modulo (+ a b) c)) -536870911 -1 -10)

(test-call "10" (lambda (a b c) (modulo a (+ b c))) 10 536870911 1)
(test-call "-536870902" (lambda (a b c) (modulo a (+ b c))) 10 -536870911 -1)
(test-call "536870902" (lambda (a b c) (modulo a (+ b c))) -10 536870911 1)
(test-call "-10" (lambda (a b c) (modulo a (+ b c))) -10 -536870911 -1)

(test-call "1"
           (lambda (a b c d) (modulo (+ a b) (+ c d)))
           536870911 2 536870911 1)
(test-call "-536870911"
           (lambda (a b c d) (modulo (+ a b) (+ c d)))
           536870911 2 -536870911 -1)
(test-call "536870911"
           (lambda (a b c d) (modulo (+ a b) (+ c d)))
           -536870911 -2 536870911 1)
(test-call "-1"
           (lambda (a b c d) (modulo (+ a b) (+ c d)))
           -536870911 -2 -536870911 -1)

(test-call "8" (lambda (a b) (logand a b)) #b1100 #b1010)
(test-call "14" (lambda (a b) (logior a b)) #b1100 #b1010)
(test-call "6" (lambda (a b) (logxor a b)) #b1100 #b1010)
(test-call "4" (lambda (a b) (logand a (lognot b))) #b1100 #b1010)

(test-call "(1 . 2)" (lambda (a b) (cons a b)) 1 2)
(test-call "1" (lambda (a) (car a)) '(1 . 2))
(test-call "2" (lambda (a) (cdr a)) '(1 . 2))

(test-call "#(1 2 3)" (lambda (a b c) (vector a b c)) 1 2 3)
(test-call "3" (lambda (v) (vector-ref v 2)) #(1 2 3))
(test-call "2" (lambda (v idx) (vector-ref v idx)) #(1 2 3) 1)
(test-call "#(42 42 42)" (lambda (n) (make-vector n 42)) 3)

(test-call "42" (lambda (n t) (when t (set! n 42)) n) 10 #t)
(test-call "10" (lambda (n t) (when t (set! n 42)) n) 10 #f)

(test-call "10" (lambda (bv) (bytevector-length bv))
           #vu8(0 1 2 3 4 5 6 7 8 9))

(test-call "52" (lambda (f) ((f 42))) (lambda (n) (lambda () (+ n 10))))

(test-call "8" (lambda (bv) (bytevector-u8-ref bv 8))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "8" (lambda (bv) (bytevector-s8-ref bv 8))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "-9" (lambda (bv) (bytevector-s8-ref bv 9))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "247" (lambda (bv) (bytevector-u8-ref bv 9))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))

(test-call "65280" (lambda (bv) (bytevector-u16-native-ref bv 0))
           #vu8(#x00 #xff #xff #x00))
(test-call "65535" (lambda (bv) (bytevector-u16-native-ref bv 1))
           #vu8(#x00 #xff #xff #x00))
(test-call "255" (lambda (bv) (bytevector-u16-native-ref bv 2))
           #vu8(#x00 #xff #xff #x00))
(test-call "-256" (lambda (bv) (bytevector-s16-native-ref bv 0))
           #vu8(#x00 #xff #xff #x00))
(test-call "-1" (lambda (bv) (bytevector-s16-native-ref bv 1))
           #vu8(#x00 #xff #xff #x00))
(test-call "255" (lambda (bv) (bytevector-s16-native-ref bv 2))
           #vu8(#x00 #xff #xff #x00))

(test-call "50463231" (lambda (bv) (bytevector-u32-native-ref bv 0))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "67305985" (lambda (bv) (bytevector-u32-native-ref bv 1))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "4278452994" (lambda (bv) (bytevector-u32-native-ref bv 2))
;;            #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "50463231" (lambda (bv) (bytevector-s32-native-ref bv 0))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "67305985" (lambda (bv) (bytevector-s32-native-ref bv 1))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "-16514302" (lambda (bv) (bytevector-s32-native-ref bv 2))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))

(test-call "511" (lambda (bv) (bytevector-u64-native-ref bv 0))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "1" (lambda (bv) (bytevector-u64-native-ref bv 1))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "72057594037927936"
;;            (lambda (bv) (bytevector-u64-native-ref bv 2))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; (test-call "18374967954648334336"
;;            (lambda (bv) (bytevector-u64-native-ref bv 3))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "511" (lambda (bv) (bytevector-s64-native-ref bv 0))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "1" (lambda (bv) (bytevector-s64-native-ref bv 1))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "72057594037927936"
;;            (lambda (bv) (bytevector-s64-native-ref bv 2))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; (test-call "-71776119061217280"
;;            (lambda (bv) (bytevector-s64-native-ref bv 3))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "-65025" (lambda (bv) (bytevector-s64-native-ref bv 0))
           #vu8(#xff 1 #xff #xff #xff #xff #xff #xff))

(test-call "42.69" (lambda (bv)
                     (bytevector-ieee-double-native-ref bv 0))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "42.689998626708984"
           (lambda (bv)
             (bytevector-ieee-single-native-ref bv 0))
           #vu8(143 194 42 66))

(test-call "85.38"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (+ f64 f64)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "43.69"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (+ f64 1.0)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "41.69"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (- f64 1.0)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "64.035"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (* f64 1.5)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "21.345"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (/ f64 2.0)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "-57.31"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (- f64 100.0)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "57.31"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (abs (- f64 100.0))))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "6.5337584895678535"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (sqrt (abs f64))))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "42"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (floor f64)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "43"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (ceiling f64)))
           #vu8(184 30 133 235 81 88 69 64))

(test-call "(-0.9614691168217643 0.2749129633138033 -3.497358237429792)"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (list (sin f64)
                     (cos f64)
                     (tan f64))))
           #vu8(184 30 133 235 81 88 69 64))

;; Not testing fasin, facos for now because apparently Guile doesn't emit those!

(test-call "(1.5473759202633208 0.7853981633974483)"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (list (atan f64)
                     (atan f64 f64))))
           #vu8(184 30 133 235 81 88 69 64))

(test-call "3" (lambda (str) (string-length str)) "fox")
(test-call "#\\f" (lambda (str) (string-ref str 0)) "fox")
(test-call "#\\x" (lambda (str) (string-ref str 2)) "fox")

(test-call "42" (lambda (box)
                  (let ((val (atomic-box-ref box)))
                    val))
           (make-atomic-box 42))

(test-call "(42 10 10 69 69 69 69 2)"
           (lambda (box)
             (let ((v0 (atomic-box-ref box)))
               (atomic-box-set! box 10)
               (let* ((v1 (atomic-box-ref box))
                      (v2 (atomic-box-swap! box 69))
                      (v3 (atomic-box-ref box))
                      (v4 (atomic-box-compare-and-swap! box 1 2))
                      (v5 (atomic-box-ref box))
                      (v6 (atomic-box-compare-and-swap! box v4 2))
                      (v7 (atomic-box-ref box)))
                 (list v0 v1 v2 v3 v4 v5 v6 v7))))
           (make-atomic-box 42))

(test-call "3" (lambda (x y) (quotient x y)) 10 3)
(test-call "-3" (lambda (x y) (quotient x y)) -10 3)
(test-call "1" (lambda (x y) (remainder x y)) 10 3)
(test-call "-1" (lambda (x y) (remainder x y)) -10 3)
(test-call "1" (lambda (x y) (modulo x y)) 10 3)
(test-call "2" (lambda (x y) (modulo x y)) -10 3)

(test-call "0" (lambda (x n) (ash x n)) 1 -2)
(test-call "-1" (lambda (x n) (ash x n)) -1 -1)
(test-call "16" (lambda (x n) (ash x n)) 32 -1)
(test-call "0" (lambda (x n) (ash x n)) 32 -64)

(test-call "4" (lambda (x n) (ash x n)) 1 2)
(test-call "-2" (lambda (x n) (ash x n)) -1 1)
(test-call "64" (lambda (x n) (ash x n)) 32 1)

(test-call "-2" (lambda (x) (ash x 1)) -1)
(test-call "-1" (lambda (x) (ash x -1)) -1)

(test-call "()" (lambda args args))
(test-call "(1)" (lambda args args) 1)
(test-call "(1 2)" (lambda args args) 1 2)
(test-call "(1 2 3)" (lambda args args) 1 2 3)
(test-call "(1 2 3 4)" (lambda args args) 1 2 3 4)
(test-call "(1 2 3 4 5)" (lambda args args) 1 2 3 4 5)
(test-call "(1 2 3 4 5 6)" (lambda args args) 1 2 3 4 5 6)
(test-call "(1 2 3 4 5 6 7)" (lambda args args) 1 2 3 4 5 6 7)
(test-call "(1 2 3 4 5 6 7 8)" (lambda args args) 1 2 3 4 5 6 7 8)
(test-call "(1 2 3 4 5 6 7 8 9)" (lambda args args) 1 2 3 4 5 6 7 8 9)
(test-call "(1 2 3 4 5 6 7 8 9 10)" (lambda args args) 1 2 3 4 5 6 7 8 9 10)

(test-call "(1 2)" (lambda (a . args) (cons* a args)) 1 2)
(test-call "(1 2 3)" (lambda (a b . args) (cons* a b args)) 1 2 3)
(test-call "(1 2 3 4)" (lambda (a b c . args) (cons* a b c args)) 1 2 3 4)
(test-call "(1 2 3 4 5)" (lambda (a b c d . args) (cons* a b c d args)) 1 2 3 4 5)
(test-call "(1 2 3 4 5 6 7 8 9 10 11 12)"
           (lambda (a b c d e f g h i j . args)
             (cons* a b c d e f g h i j args))
           1 2 3 4 5 6 7 8 9 10 11 12)

(test-call "false" (lambda* (#:optional a) a))
(test-call "(42 69)" (lambda* (#:optional (a 42) (b 69)) (list a b)))
(test-call "(10 20)" (lambda* (#:optional (a 42) (b 69)) (list a b)) 10 20)

(test-call "(1 false ())" (lambda* (a #:optional b . rest) (list a b rest)) 1)

(test-call "(1 2 3 4 5 6 7 8 9 10 11 12)"
           (lambda* (a b c d #:optional e f g h i j . args)
             (cons* a b c d e f g h i j args))
           1 2 3 4 5 6 7 8 9 10 11 12)

(test-call "(1 2 3 4 5 6 false false false false)"
           (lambda* (a b c d #:optional e f g h i j . args)
             (cons* a b c d e f g h i j args))
           1 2 3 4 5 6)

(test-call "20" (lambda (f . args) (apply f args)) (lambda (x y) (+ x y)) 12 8)
(test-call "12\n8" (lambda (f . args) (apply f args)) values 12 8)

(test-call "42" (lambda (f tag)
                  (call-with-prompt tag
                    (lambda () (f))
                    (lambda (k) #f)))
                (lambda () 42)
                "hey")

(test-call "69" (lambda (f tag)
                  (call-with-prompt tag
                    (lambda () (1+ (f tag)))
                    (lambda (k v) v)))
                (lambda (tag) (abort-to-prompt tag 69))
                "hey")

(test-call "69"
           (lambda (abort-to-prompt tag)
             (call-with-prompt tag
                               (lambda ()
                                 (dynamic-wind values
                                     (lambda () 42)
                                     (lambda () (abort-to-prompt tag 69))))
                               (lambda (k v) v)))
           abort-to-prompt "hey")
(test-call "69"
           (lambda (abort-to-prompt tag)
             (call-with-prompt tag
                               (lambda ()
                                 (dynamic-wind values
                                     (lambda () (abort-to-prompt tag 42))
                                     (lambda () (abort-to-prompt tag 69))))
                               (lambda (k v) v)))
           abort-to-prompt "hey")

(test-call "69" (lambda (f tag)
                  (call-with-prompt tag
                    (lambda () (- (f tag)))
                    (lambda (k v) (k (- -2 v)))))
                (lambda (tag) (abort-to-prompt tag 67))
                "hey")

(test-call "(42 (69 100) 42)"
           (lambda (f)
             (let* ((fluid (make-fluid 42))
                    (a (fluid-ref fluid))
                    (b (with-fluid* fluid 69 (lambda () (f fluid))))
                    (c (fluid-ref fluid)))
               (list a b c)))
           (lambda (fluid)
             (let ((v (fluid-ref fluid)))
               (fluid-set! fluid 100)
               (list v (fluid-ref fluid)))))

(test-call "#vu8(0 0 0 0 0)"
           (lambda () (make-bytevector 5)))
(test-call "#vu8(42 42 42 42 42)"
           (lambda () (make-bytevector 5 42)))
(test-call "#vu8(1 2 3 4)"
           (lambda () (bytevector 1 2 3 4)))

(test-call "79" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 69)))))
(test-call "52" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise-continuable 69))))))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise-continuable 69)))
                   #:unwind? #t)))
(test-call "69" (lambda ()
                  (with-exception-handler
                   (lambda (exn) exn)
                   (lambda () (+ 10 (raise-continuable 69)))
                   #:unwind? #t)))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (+ 10 (raise 69)))
                   #:unwind? #t)))
(test-call "69" (lambda ()
                  (with-exception-handler
                   (lambda (exn) exn)
                   (lambda () (+ 10 (raise 69)))
                   #:unwind? #t)))
(test-call "42" (lambda ()
                  (with-exception-handler
                   (lambda (exn) 42)
                   (lambda () (error "what"))
                   #:unwind? #t)))

(test-call "#vu8(100 120)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-u8 100 p)
               (write-u8 120 p)
               (get-output-bytevector p))))

(test-call "#vu8(100 120 130 140)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-bytevector #vu8(100 120) p)
               (write-bytevector #vu8(130 140) p)
               (get-output-bytevector p))))

(test-call "#vu8(104 101 108 108 111 44 32 119 111 114 108 100)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-string "hello, world" p)
               (get-output-bytevector p))))

(test-call "#(1 1 2 3 #eof #eof #eof)"
           (lambda ()
             (let* ((p (open-input-bytevector #vu8(1 2 3)))
                    (a (peek-u8 p))
                    (b (read-u8 p))
                    (c (read-u8 p))
                    (d (read-u8 p))
                    (e (read-u8 p))
                    (f (peek-u8 p))
                    (g (read-u8 p)))
               (vector a b c d e f g))))

(test-call "#(#vu8() #vu8(1) #vu8(1 2) #vu8(1 2 3) #vu8(1 2 3))"
           (lambda ()
             (define (read-n n)
               (read-bytevector n (open-input-bytevector #vu8(1 2 3))))
             (vector (read-n 0)
                     (read-n 1)
                     (read-n 2)
                     (read-n 3)
                     (read-n 4))))

(test-call "#eof"
           (lambda ()
             (read-bytevector 1 (open-input-bytevector #vu8()))))

(test-call "#(#\\h #\\h #\\e #\\l #\\l #\\o #eof #eof #eof)"
           (lambda ()
             (let* ((p (open-input-bytevector #vu8(104 101 108 108 111)))
                    (a (peek-char p))
                    (b (read-char p))
                    (c (read-char p))
                    (d (read-char p))
                    (e (read-char p))
                    (f (read-char p))
                    (g (read-char p))
                    (h (peek-char p))
                    (i (read-char p)))
               (vector a b c d e f g h i))))

(test-call "#( h he hel hell hello hello)"
           (lambda ()
             (define (read-n n)
               (read-string n (open-input-bytevector #vu8(104 101 108 108 111))))
             (vector (read-n 0)
                     (read-n 1)
                     (read-n 2)
                     (read-n 3)
                     (read-n 4)
                     (read-n 5)
                     (read-n 6))))

;; 
;; This is how you would debug outside the test suite...
;; (call-with-compiled-wasm-file
;;  (compile '(lambda (n)
;;              (let fib ((n n))
;;                          (if (<= n 1)
;;                              1
;;                              (+ (fib (- n 1)) (fib (- n 2))))))
;;           #:import-abi? #f #:export-abi? #t #:dump-cps? #t #:dump-wasm? #t)
;;  (lambda (proc)
;;    (call-with-compiled-wasm-file
;;     (compile 40 #:import-abi? #t #:export-abi? #f)
;;     (lambda (arg)
;;       (copy-file proc "/tmp/proc.wasm")
;;       (copy-file arg "/tmp/arg.wasm")
;;       (pk (run-d8 "test-call.js" "--" proc arg))))))

(when (and (batch-mode?)
           (or (not (zero? (test-runner-fail-count (test-runner-get))))
               (not (zero? (test-runner-xpass-count (test-runner-get))))))
  (force-output)
  (exit 1))

(test-end "test-constants")
