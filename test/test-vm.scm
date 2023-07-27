;;; WebAssembly virtual machine
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
;;; Virtual machine tests.
;;;
;;; Code:

(use-modules (ice-9 binary-ports)
             (ice-9 exceptions)
             (ice-9 popen)
             (ice-9 textual-ports)
             (srfi srfi-64)
             (wasm assemble)
             (wasm vm))

(define s32-overflow (@@ (wasm vm) s32-overflow))

(define d8 (or (getenv "D8") "d8"))
(define srcdir (or (getenv "SRCDIR") (getcwd)))

(define (scope-file file-name)
  (string-append srcdir "/" file-name))

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

(define (call-with-wasm-file wasm f)
  (let* ((wasm-port (mkstemp "/tmp/tmp-wasm-XXXXXX"))
         (wasm-file-name (port-filename wasm-port)))
    (put-bytevector wasm-port wasm)
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
         (result (string-trim-both (get-string-all port))))
    (if (zero? (close-pipe port))
        (call-with-input-string result read)
        (error result))))

(define (run-wasm-in-d8 wasm func args)
  (call-with-wasm-file
   wasm
   (lambda (wasm-file-name)
     (apply run-d8 (scope-file "test/load-wasm-and-print-primitive.js")
            "--" wasm-file-name func
            (map (lambda (arg) (format #f "~a" arg)) args)))))

(define (run-wasm-in-vm wasm func args imports)
  (let* ((module (make-wasm-module wasm))
         (instance (make-wasm-instance module #:imports imports)))
    (apply (wasm-instance-export-ref instance func) args)))

(define (eval-wat wat func args imports d8?)
  (let* ((wasm (wat->wasm wat))
         (our-result (run-wasm-in-vm wasm func args imports)))
    (when d8?
      (let ((d8-result (run-wasm-in-d8 wasm func args)))
        (unless (equal? our-result d8-result)
          (error "our result differs from d8" our-result d8-result))))
    our-result))

(define* (test-vm name expected wat #:key
                  (func "main") (args '()) (imports '()) (d8? #t))
  (let ((wasm (wat->wasm wat)))
    (test-equal name
      expected
      (eval-wat wat func args imports d8?))))

(test-begin "test-vm")

(test-vm "i32.eqz true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.eqz (i32.const 0)))))

(test-vm "i32.eqz false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.eqz (i32.const 42)))))

(test-vm "i32.eq true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.eq (i32.const 42) (i32.const 42)))))

(test-vm "i32.eq false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.eq (i32.const 42) (i32.const 7)))))

(test-vm "i32.ne true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.ne (i32.const 42) (i32.const 7)))))

(test-vm "i32.ne false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.ne (i32.const 42) (i32.const 42)))))

(test-vm "i32.lt_s true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.lt_s (i32.const -42) (i32.const 42)))))

(test-vm "i32.lt_s false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.lt_s (i32.const 42) (i32.const -42)))))

(test-vm "i32.lt_u true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.lt_s (i32.const 7) (i32.const 42)))))

(test-vm "i32.lt_u false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.lt_s (i32.const 42) (i32.const 7)))))

(test-vm "i32.le_s true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.le_s (i32.const 42) (i32.const 42)))))

(test-vm "i32.le_s false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.le_s (i32.const 42) (i32.const -42)))))

(test-vm "i32.le_u true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.le_s (i32.const 42) (i32.const 42)))))

(test-vm "i32.le_u false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.le_s (i32.const 42) (i32.const 7)))))

(test-vm "i32.gt_s true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.gt_s (i32.const 42) (i32.const -42)))))

(test-vm "i32.gt_s false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.gt_s (i32.const -42) (i32.const 42)))))

(test-vm "i32.gt_u true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.gt_s (i32.const 42) (i32.const 7)))))

(test-vm "i32.gt_u false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.gt_s (i32.const 7) (i32.const 42)))))

(test-vm "i32.ge_s true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.ge_s (i32.const 42) (i32.const 42)))))

(test-vm "i32.ge_s false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.ge_s (i32.const -42) (i32.const 42)))))

(test-vm "i32.ge_u true"
         1
         '(module
           (func (export "main") (result i32)
                 (i32.ge_s (i32.const 42) (i32.const 42)))))

(test-vm "i32.ge_u false"
         0
         '(module
           (func (export "main") (result i32)
                 (i32.ge_s (i32.const 7) (i32.const 42)))))

(test-vm "i32.add"
         80
         '(module
           (func (export "main") (result i32)
                 (i32.add (i32.const 42) (i32.const 38)))))

(test-vm "i32.sub"
         4
         '(module
           (func (export "main") (result i32)
                 (i32.sub (i32.const 42) (i32.const 38)))))

(test-vm "i32.mul"
         42
         '(module
           (func (export "main") (result i32)
                 (i32.mul (i32.const 7) (i32.const 6)))))

(test-vm "i32.div_s"
         -6
         '(module
           (func (export "main") (result i32)
                 (i32.div_s (i32.const -42) (i32.const 7)))))

(test-vm "i32.div_u"
         6
         '(module
           (func (export "main") (result i32)
                 (i32.div_u (i32.const 42) (i32.const 7)))))

(test-vm "i32.rem_s"
         -2
         '(module
           (func (export "main") (result i32)
                 (i32.rem_s (i32.const -42) (i32.const 5)))))

(test-vm "i32.rem_u"
         2
         '(module
           (func (export "main") (result i32)
                 (i32.rem_u (i32.const 42) (i32.const 5)))))

(test-vm "i32.and"
         #b1010
         '(module
           (func (export "main") (result i32)
                 (i32.and (i32.const #b1111) (i32.const #b1010)))))

(test-vm "i32.or"
         #b1111
         '(module
           (func (export "main") (result i32)
                 (i32.or (i32.const #b0101) (i32.const #b1010)))))

(test-vm "i32.xor"
         #b1110
         '(module
           (func (export "main") (result i32)
                 (i32.or (i32.const #b1010) (i32.const #b0100)))))

(test-vm "i32.shl"
         #b1000
         '(module
           (func (export "main") (result i32)
                 (i32.shl (i32.const #b0001) (i32.const 3)))))

(test-vm "i32.shr_s"
         -2
         '(module
           (func (export "main") (result i32)
                 (i32.shr_s (i32.const -16) (i32.const 3)))))

(test-vm "i32.shr_u"
         #b0001
         '(module
           (func (export "main") (result i32)
                 (i32.shr_u (i32.const #b1000) (i32.const 3)))))

(test-vm "i32.rotl"
         (s32-overflow #b11000011110000111100001111000011)
         `(module
           (func (export "main") (result i32)
                 (i32.rotl (i32.const
                            ,(s32-overflow #b11110000111100001111000011110000))
                           (i32.const 2)))))

(test-vm "i32.rotr"
         (s32-overflow #b00111100001111000011110000111100)
         `(module
           (func (export "main") (result i32)
                 (i32.rotr (i32.const
                            ,(s32-overflow #b11110000111100001111000011110000))
                           (i32.const 2)))))

(test-vm "i32.clz"
         7
         `(module
           (func (export "main") (result i32)
                 (i32.clz (i32.const ,(ash 1 24))))))

(test-vm "i32.ctz"
         24
         `(module
           (func (export "main") (result i32)
                 (i32.ctz (i32.const ,(ash 1 24))))))

(test-vm "i32.popcnt"
         16
         `(module
           (func (export "main") (result i32)
                 (i32.popcnt (i32.const ,(s32-overflow #xaaaaAAAA))))))

(test-vm "i32.store + i32.load"
         42
         '(module
           (memory 1)
           (func (export "main") (result i32)
                 (i32.store (i32.const 0) (i32.const 42))
                 (i32.load (i32.const 0)))))

(test-vm "i32.store16 + i32.load16_s"
         -42
         '(module
           (memory $memory 1)
           (func (export "main") (result i32)
                 (i32.store16 (i32.const 0) (i32.const -42))
                 (i32.load16_s (i32.const 0)))))

(test-vm "i32.store16 + i32.load16_s wrap"
         -1
         '(module
           (memory $memory 1)
           (func (export "main") (result i32)
                 (i32.store16 (i32.const 0) (i32.const -65537))
                 (i32.load16_s (i32.const 0)))))

(test-vm "i32.store16 + i32.load16_u"
         65535
         '(module
           (memory $memory 1)
           (func (export "main") (result i32)
                 (i32.store16 (i32.const 0) (i32.const 65535))
                 (i32.load16_u (i32.const 0)))))

(test-vm "i32.store16 + i32.load16_u wrap"
         65535
         '(module
           (memory $memory 1)
           (func (export "main") (result i32)
                 (i32.store16 (i32.const 0) (i32.const -65537))
                 (i32.load16_u (i32.const 0)))))

(test-vm "i32.store8 + i32.load8_s"
         -42
         '(module
           (memory $memory 1)
           (func (export "main") (result i32)
                 (i32.store8 (i32.const 0) (i32.const -42))
                 (i32.load8_s (i32.const 0)))))

(test-vm "i32.store8 + i32.load8_s wrap"
         -1
         '(module
           (memory $memory 1)
           (func (export "main") (result i32)
                 (i32.store8 (i32.const 0) (i32.const -257))
                 (i32.load8_s (i32.const 0)))))

(test-vm "i32.store8 + i32.load8_u"
         255
         '(module
           (memory $memory 1)
           (func (export "main") (result i32)
                 (i32.store8 (i32.const 0) (i32.const 255))
                 (i32.load8_u (i32.const 0)))))

(test-vm "i32.store8 + i32.load8_u wrap"
         255
         '(module
           (memory $memory 1)
           (func (export "main") (result i32)
                 (i32.store8 (i32.const 0) (i32.const -257))
                 (i32.load8_u (i32.const 0)))))

(test-vm "memory.size"
         1
         '(module
           (memory 1)
           (func (export "main") (result i32)
                 (memory.size))))

(test-vm "memory.grow within limits"
         2
         '(module
           (memory 2 3)
           (func (export "main") (result i32)
                 (i32.const 1)
                 (memory.grow))))

(test-vm "memory.grow outside limits"
         -1
         '(module
           (memory 1 1)
           (func (export "main") (result i32)
                 (i32.const 1)
                 (memory.grow))))

(test-vm "memory.grow no-op"
         1
         '(module
           (memory 1 1)
           (func (export "main") (result i32)
                 (i32.const 0)
                 (memory.grow))))

(test-vm "if"
         37
         '(module
           (func (export "main") (result i32)
                 (if (result i32)
                     (i32.eq (i32.const 42) (i32.const 47))
                     (then (i32.const 13))
                     (else (i32.const 37))))))

(test-vm "branching in a block"
         42
         '(module
           (func (export "main") (result i32)
                 (block $foo
                        (br $foo)
                        (unreachable))
                 (i32.const 42))))

(test-vm "return"
         42
         '(module
           (func (export "main") (result i32)
                 (block $foo (result i32)
                        (i32.const 42)
                        (return)
                        (unreachable)))))

(test-vm "return via br"
         42
         '(module
           (func (export "main") (result i32)
                 (i32.const 42)
                 (br 0)
                 (unreachable))))

(test-vm "return with extra values on stack"
         42
         '(module
           (func (export "main") (result i32)
                 (block $foo (result i32)
                        (i32.const 13)
                        (i32.const 42)
                        (return)))))

(test-assert "fallthrough with too many values on stack"
  (with-exception-handler (lambda (e) #t)
    (lambda ()
      (let ((wat '(module
                   (func (export "main") (result i32)
                         (i32.const 1)
                         (i32.const 2)))))
        (eval-wat wat "main" '() '() #f)
        #f))
    #:unwind? #t
    #:unwind-for-type &wasm-validation-error))

(test-assert "fallthrough with too many values on invalid stack"
  (with-exception-handler (lambda (e) #t)
    (lambda ()
      (let ((wat '(module
                   (func (export "main") (result i32)
                         (i32.const 1)
                         (return) ; stack is invalid after this
                         (i32.const 2)
                         (i32.const 3)))))
        (eval-wat wat "main" '() '() #f)
        #f))
    #:unwind? #t
    #:unwind-for-type &wasm-validation-error))

(test-vm "branching in a loop"
         24
         '(module
           ;; Iterative factorial.
           (func (export "main") (param $n i32) (result i32)
                 (local $result i32)
                 (local.set $result (i32.const 1))
                 (loop $loop (result i32)
                       (if (result i32)
                           (i32.eq (local.get $n) (i32.const 1))
                           (then (local.get $result))
                           (else (local.set $result (i32.mul (local.get $n)
                                                             (local.get $result)))
                                 (local.set $n (i32.sub (local.get $n) (i32.const 1)))
                                 (br $loop))))))
         #:args '(4))

(test-vm "blocks with params and results"
         24
         '(module
           (func (export "main") (param $n i32) (result i32)
                 (local.get $n)
                 (block $block (param i32) (result i32)
                        (loop $loop (param i32) (result i32)
                              (i32.eq (local.get $n) (i32.const 1))
                              (br_if $block)
                              (local.tee $n (i32.sub (local.get $n) (i32.const 1)))
                              (i32.mul)
                              (br $loop)))))
         #:args '(4))

(test-vm "inner function call"
         11
         '(module
           (func $y (param $m i32) (param $x i32) (param $b i32) (result i32)
                 (i32.add (i32.mul (local.get $m) (local.get $x)) (local.get $b)))
           (func (export "main") (param $x i32) (result i32)
                 (call $y (i32.const 2) (local.get $x) (i32.const 3))))
         #:args '(4))

(test-vm "imported function call"
         80
         '(module
           (func $add42 (import "lib" "add42")
                 (param i32) (result i32))
           (func (export "main") (param $x i32) (result i32)
                 (call $add42 (local.get $x))))
         #:imports `(("lib" . (("add42" . ,(lambda (x) (+ x 42))))))
         #:args '(38)
         #:d8? #f)

(test-vm "mutable globals"
         42
         '(module
           (global $foo (mut i32) (i32.const 30))
           (func (export "main") (param $n i32) (result i32)
                 (global.set $foo (i32.add (local.get $n) (global.get $foo)))
                 (global.get $foo)))
         #:args '(12))

(test-vm "imported globals"
         80
         '(module
           (global $foo i32 (i32.const 42))
           (global $bar (import "globals" "bar") i32)
           (func (export "main") (result i32)
                 (i32.add (global.get $foo) (global.get $bar))))
         #:imports `(("globals" . (("bar" . ,(make-wasm-global 38 #f)))))
         #:d8? #f)

(test-vm "drop"
         42
         '(module
           (func (export "main") (result i32)
                 (i32.const 42)
                 (i32.const 13)
                 (drop))))

(test-vm "drop in unreachable code"
         42
         '(module
           (func (export "main") (result i32)
                 (i32.const 42)
                 (return)
                 (drop))))

(test-vm "select first"
         42
         '(module
           (func (export "main") (result i32)
                 (i32.const 42)
                 (i32.const 13)
                 (i32.const 1)
                 (select))))

(test-vm "select second"
         13
         '(module
           (func (export "main") (result i32)
                 (i32.const 42)
                 (i32.const 13)
                 (i32.const 0)
                 (select))))

(test-vm "select in unreachable code"
         42
         '(module
           (func (export "main") (result i32)
                 (i32.const 42)
                 (return)
                 (select))))

(test-vm "select in unreachable code with i32 on top"
         42
         '(module
           (func (export "main") (result i32)
                 (i32.const 42)
                 (return)
                 (i32.const 0)
                 (select))))

(test-vm "start function"
         42
         '(module
           (global $foo (mut i32) (i32.const 13))
           (func $init
                 (global.set $foo (i32.const 42)))
           (start $init)
           (func (export "main") (result i32)
                 (global.get $foo))))


(when (and (batch-mode?)
           (or (not (zero? (test-runner-fail-count (test-runner-get))))
               (not (zero? (test-runner-xpass-count (test-runner-get))))))
  (exit 1))

(test-end "test-vm")
