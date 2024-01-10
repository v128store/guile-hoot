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
;;; Disassembler tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils)
             (wasm wat))

(test-begin "test-disassemble")

(test-equal "disassemble"
  '(module
    $bloop
    (func $cos (import "math" "cos") (param f64) (result f64))
    (table $objs 10 (ref eq))
    (memory $main 1)
    (global $the-answer i32 (i32.const 42))
    (global $mutable (mut i32) (i32.const 0))
    (elem $stuff (table $objs) (offset (i32.const 0)) (ref i31)
          (item (i32.const 42) (ref.i31)))
    (data $bytes $main (i32.const 0) #vu8(1 2 3 4))
    (func $factorial (export "factorial") (param $n i32) (result i32)
          (local $result i32)
          (i32.const 1)
          (local.set $result)
          (loop $loop (result i32)
                (local.get $n)
                (i32.const 1)
                (i32.eq)
                (if (result i32)
                    (then (local.get $result))
                    (else (local.get $n)
                          (local.get $result)
                          (i32.mul)
                          (local.set $result)
                          (local.get $n)
                          (i32.const 1)
                          (i32.sub)
                          (local.set $n)
                          (br $loop)))))
    (func $init (i32.const 1337) (global.set $mutable)))
  (wasm->wat
   (wat->wasm
    '(module
      $bloop
      (func $cos (import "math" "cos") (param f64) (result f64))
      (global $the-answer i32 (i32.const 42))
      (global $mutable (mut i32) (i32.const 0))
      (table $objs 10 (ref eq))
      (memory $main 1)
      (elem $stuff (table $objs) (offset (i32.const 0)) (ref i31)
            (item (ref.i31 (i32.const 42))))
      (data $bytes (memory $main) (i32.const 0) #vu8(1 2 3 4))
      (func $factorial (export "factorial") (param $n i32) (result i32)
            (local $result i32)
            (local.set $result (i32.const 1))
            (loop $loop (result i32)
                  (if (result i32)
                      (i32.eq (local.get $n) (i32.const 1))
                      (then (local.get $result))
                      (else (local.set $result (i32.mul (local.get $n)
                                                        (local.get $result)))
                            (local.set $n (i32.sub (local.get $n)
                                                   (i32.const 1)))
                            (br $loop)))))
      (func $init
            (global.set $mutable (i32.const 1337)))))))

(test-end* "test-disassemble")
