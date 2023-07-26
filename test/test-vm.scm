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
             (srfi srfi-64)
             (wasm assemble)
             (wasm vm))

(define s32-overflow (@@ (wasm vm) s32-overflow))

(define (eval-wat wat func args imports)
  (let* ((wasm (wat->wasm wat))
         (module (make-wasm-module wasm))
         (instance (make-wasm-instance module #:imports imports)))
    (apply (wasm-instance-export-ref instance func) args)))

(define* (test-vm name expected wat #:key
                  (func "main") (args '()) (imports '()))
  (test-equal name expected (eval-wat wat func args imports)))

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
        (eval-wat wat "main" '() '())
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
        (eval-wat wat "main" '() '())
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
         80
         '(module
           (func $add-42 (param $x i32) (result i32)
                 (i32.add (local.get $x) (i32.const 42)))
           (func (export "main") (param $x i32) (result i32)
                 (call $add-42 (local.get $x))))
         #:args '(38))

(test-vm "imported function call"
         80
         '(module
           (func $add42 (import "lib" "add42")
                 (param i32) (result i32))
           (func (export "main") (param $x i32) (result i32)
                 (call $add42 (local.get $x))))
         #:imports `(("lib" . (("add42" . ,(lambda (x) (+ x 42))))))
         #:args '(38))

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
         #:imports `(("globals" . (("bar" . ,(make-wasm-global 38 #f))))))

(when (and (batch-mode?)
           (or (not (zero? (test-runner-fail-count (test-runner-get))))
               (not (zero? (test-runner-xpass-count (test-runner-get))))))
  (exit 1))

(test-end "test-vm")
