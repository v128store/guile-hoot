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
;;; String tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (wasm wat)
             (wasm lower)
             (test utils))

(test-begin "test-lower")

(let ((mod (wat->wasm
            '((func $string-to-i32 (import "app" "string-to-i32")
                    (param (ref string)) (result i32))
              (func $i32-to-string (import "app" "i32-to-string")
                    (param i32) (result (ref string)))
              (func $main (param $i i32) (result i32)
                    (local.get $i)
                    (call $i32-to-string)
                    (call $string-to-i32))
              (global $s0 (ref string) (string.const "hey"))))))
  (test-equal '(module
                (type #f (func (param (ref string)) (result i32)))
                (type #f (func (param i32) (result (ref string))))
                (type #f (func (param $i i32) (result i32)))
                (import "app" "string-to-i32"
                        (func $string-to-i32 (param (ref string)) (result i32)))
                (import "app" "i32-to-string"
                        (func $i32-to-string (param i32) (result (ref string))))
                (global $s0 (ref string) (string.const "hey"))
                (func $main (param $i i32) (result i32)
                      (local.get 0) (call 1) (call 0)))
              (wasm->wat (lower-wasm mod #:stringref-lowering 'stringref))))

(let ((mod (wat->wasm
            '((func $string-to-i32 (import "app" "string-to-i32")
                    (param (ref string)) (result i32))
              (func $i32-to-string (import "app" "i32-to-string")
                    (param i32) (result (ref string)))
              (func $main (param $i i32) (result i32)
                    (local.get $i)
                    (call $i32-to-string)
                    (call $string-to-i32))
              (global $s0 (ref string) (string.const "hey"))))))
  (test-equal '(module
                (type $wtf8 (array (mut i8)))
                (type $__start (func))
                (type #f (func (param (ref extern)) (result i32)))
                (type #f (func (param i32) (result (ref extern))))
                (type #f (func (param $wtf8 (ref null 0)) (result (ref extern))))
                (type #f (func (param $str (ref null extern)) (result (ref 0))))
                (type #f (func (param (ref 0)) (result i32)))
                (type #f (func (param i32) (result (ref 0))))
                (type #f (func (param $i i32) (result i32)))
                (import "app" "string-to-i32"
                        (func $string-to-i32-stringref-0
                              (param (ref extern)) (result i32)))
                (import "app" "i32-to-string"
                        (func $i32-to-string-stringref-1
                              (param i32) (result (ref extern))))
                (import "rt" "wtf8_to_string"
                        (func $wtf8->extern-string
                              (param $wtf8 (ref null 0)) (result (ref extern))))
                (import "rt" "string_to_wtf8"
                        (func $extern-string->wtf8
                              (param $str (ref null extern)) (result (ref 0))))
                (global $stringref-2 (mut (ref null 0)) (ref.null 0))
                (global $s0 (mut (ref null 0)) (ref.null 0))
                (data $stringref-2 #vu8(104 101 121))
                (func $string-to-i32 (param (ref 0)) (result i32)
                      (local #f i32)
                      (local.get 0)
                      (call 2)
                      (call 0)
                      (local.set 0)
                      (local.get 0))
                (func $i32-to-string (param i32) (result (ref 0))
                      (local #f (ref extern))
                      (local.get 0)
                      (call 1)
                      (local.set 0)
                      (local.get 0)
                      (call 3))
                (func $main (param $i i32) (result i32)
                      (local.get 0) (call 5) (call 4))
                (func $__start
                      (i32.const 0)
                      (i32.const 3)
                      (array.new_data 0 0)
                      (global.set 0)
                      (global.get 0)
                      (ref.as_non_null)
                      (global.set 1))
                (start 7))
              (wasm->wat (lower-wasm mod #:stringref-lowering 'wtf8))))

(test-end* "test-lower")
