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

(use-modules (test utils)
             (srfi srfi-64)
             (wasm lower-globals)
             (wasm wat))

(test-begin "test-lower-globals")

(let ((mod (wat->wasm
            '((type $i32 (struct i32))
              (func $succ (param i32) (result i32)
                    (i32.add (local.get 0) (i32.const 1)))
              (global $g0 i32 (i32.const 42))
              (global $g1 (ref $i32)
                      (struct.new $i32 (global.get $g0)))
              (global $g2 (ref $i32)
                      (struct.new $i32
                                  (call $succ
                                        (struct.get $i32 0 (global.get $g1)))))
              (global $g3 (ref $i32)
                      (struct.new $i32
                                  (call $succ
                                        (struct.get $i32 0 (global.get $g2)))))))))
  (test-equal '(module
                (type $i32 (struct i32))
                (type $__start (func))
                (global $g0 i32 (i32.const 42))
                (global $g1 (ref $i32) (global.get $g0) (struct.new $i32))
                (global $g2 (mut (ref null $i32)) (ref.null $i32))
                (global $g3 (mut (ref null $i32)) (ref.null $i32))
                (func $succ (param i32) (result i32)
                      (local.get 0) (i32.const 1) (i32.add))
                (func $__start
                      (global.get $g1) (struct.get $i32 0)
                      (call $succ) (struct.new $i32) (global.set $g2)
                      (global.get $g2) (ref.as_non_null) (struct.get $i32 0)
                      (call $succ) (struct.new $i32) (global.set $g3))
                (start $__start))
              (wasm->wat (lower-globals mod))))

(test-end* "test-lower-globals")
