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
             (wasm lower-stringrefs)
             (test utils))

(test-begin "test-lower-stringrefs")

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
                (func $string-to-i32-stringref-0 (import "app" "string-to-i32")
                      (param (ref extern)) (result i32))
                (func $i32-to-string-stringref-1 (import "app" "i32-to-string")
                      (param i32) (result (ref extern)))
                (func $wtf8->extern-string (import "rt" "wtf8_to_string")
                      (param $wtf8 (ref null $wtf8)) (result (ref extern)))
                (func $extern-string->wtf8 (import "rt" "string_to_wtf8")
                      (param $str (ref null extern)) (result (ref $wtf8)))
                ;; This is an odd lowering: sure, it's correct, but why
                ;; two globals?  But having a global which is itself a
                ;; string.const is pretty unusual, we think:
                ;; string.const is usually used inline.  It's not
                ;; important to optimize this case; the common logic to
                ;; intern a unique global for each unique operand of
                ;; string.const is OK.
                (global $stringref-2 (ref $wtf8)
                        (i32.const 0) (i32.const 3) (array.new_data $wtf8 $stringref-2))
                (global $s0 (ref $wtf8) (global.get $stringref-2))
                (data $stringref-2 #vu8(104 101 121))
                (func $main
                      (param $i i32) (result i32)
                      (local.get $i)
                      (call $i32-to-string)
                      (call $string-to-i32))
                (func $string-to-i32
                      (param (ref $wtf8)) (result i32)
                      (local #f i32)
                      (local.get 0)
                      (call $wtf8->extern-string)
                      (call $string-to-i32-stringref-0)
                      (local.set 1)
                      (local.get 1))
                (func $i32-to-string
                      (param i32)
                      (result (ref $wtf8))
                      (local #f (ref extern))
                      (local.get 0)
                      (call $i32-to-string-stringref-1)
                      (local.set 1)
                      (local.get 1)
                      (call $extern-string->wtf8)))
              (wasm->wat (lower-stringrefs mod #:strategy 'wtf8))))

(test-end* "test-lower-stringrefs")
