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

(test-call "10" (lambda (bv) ((@ (rnrs bytevectors) bytevector-length) bv))
           #vu8(0 1 2 3 4 5 6 7 8 9))

(test-call "52" (lambda (f) ((f 42))) (lambda (n) (lambda () (+ n 10))))

(test-call "8" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u8-ref) bv 8))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "8" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s8-ref) bv 8))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "-9" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s8-ref) bv 9))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "247" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u8-ref) bv 9))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))

(test-call "65280" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u16-native-ref) bv 0))
           #vu8(#x00 #xff #xff #x00))
(test-call "65535" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u16-native-ref) bv 1))
           #vu8(#x00 #xff #xff #x00))
(test-call "255" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u16-native-ref) bv 2))
           #vu8(#x00 #xff #xff #x00))
(test-call "-256" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s16-native-ref) bv 0))
           #vu8(#x00 #xff #xff #x00))
(test-call "-1" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s16-native-ref) bv 1))
           #vu8(#x00 #xff #xff #x00))
(test-call "255" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s16-native-ref) bv 2))
           #vu8(#x00 #xff #xff #x00))

(test-call "50463231" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u32-native-ref) bv 0))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "67305985" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u32-native-ref) bv 1))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "4278452994" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u32-native-ref) bv 2))
;;            #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "50463231" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s32-native-ref) bv 0))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "67305985" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s32-native-ref) bv 1))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "-16514302" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s32-native-ref) bv 2))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))

(test-call "511" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u64-native-ref) bv 0))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "1" (lambda (bv) ((@ (rnrs bytevectors) bytevector-u64-native-ref) bv 1))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "72057594037927936"
;;            (lambda (bv) ((@ (rnrs bytevectors) bytevector-u64-native-ref) bv 2))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; (test-call "18374967954648334336"
;;            (lambda (bv) ((@ (rnrs bytevectors) bytevector-u64-native-ref) bv 3))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "511" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s64-native-ref) bv 0))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "1" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s64-native-ref) bv 1))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "72057594037927936"
;;            (lambda (bv) ((@ (rnrs bytevectors) bytevector-s64-native-ref) bv 2))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; (test-call "-71776119061217280"
;;            (lambda (bv) ((@ (rnrs bytevectors) bytevector-s64-native-ref) bv 3))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "-65025" (lambda (bv) ((@ (rnrs bytevectors) bytevector-s64-native-ref) bv 0))
           #vu8(#xff 1 #xff #xff #xff #xff #xff #xff))

(test-call "42.69" (lambda (bv)
                     ((@ (rnrs bytevectors) bytevector-ieee-double-native-ref) bv 0))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "42.689998626708984"
           (lambda (bv)
             ((@ (rnrs bytevectors) bytevector-ieee-single-native-ref) bv 0))
           #vu8(143 194 42 66))

(test-call "3" (lambda (str) (string-length str)) "fox")
(test-call "#\\f" (lambda (str) (string-ref str 0)) "fox")
(test-call "#\\x" (lambda (str) (string-ref str 2)) "fox")

(test-call "42" (lambda (box)
                  (define ref (@ (ice-9 atomic) atomic-box-ref))
                  (define set! (@ (ice-9 atomic) atomic-box-set!))
                  (define swap! (@ (ice-9 atomic) atomic-box-swap!))
                  (define cas! (@ (ice-9 atomic) atomic-box-compare-and-swap!))
                  (let ((val (ref box)))
                    val))
           ((@ (ice-9 atomic) make-atomic-box) 42))

(test-call "(42 10 10 69 69 69 69 2)"
           (lambda (box)
             (define ref (@ (ice-9 atomic) atomic-box-ref))
             (define set! (@ (ice-9 atomic) atomic-box-set!))
             (define swap! (@ (ice-9 atomic) atomic-box-swap!))
             (define cas! (@ (ice-9 atomic) atomic-box-compare-and-swap!))
             (let ((v0 (ref box)))
               (set! box 10)
               box
               (let* ((v1 (ref box))
                      (v2 (swap! box 69))
                      (v3 (ref box))
                      (v4 (cas! box 1 2))
                      (v5 (ref box))
                      (v6 (cas! box v4 2))
                      (v7 (ref box)))
                 (list v0 v1 v2 v3 v4 v5 v6 v7))))
           ((@ (ice-9 atomic) make-atomic-box) 42))

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
  (exit 1))

(test-end "test-constants")
