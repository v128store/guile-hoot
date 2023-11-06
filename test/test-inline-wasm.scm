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
;;; Floating point number tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-inline-wasm")

(test-call "43" (lambda (a)
                  (%inline-wasm
                   '(func (param $a i64) (result i64)
                          (i64.add (local.get $a) (i64.const 1)))
                   a))
           42)
(test-call "43" (lambda (a)
                  (%inline-wasm
                   '(func (param $a i32) (result i64)
                          (i64.extend_i32_u (i32.add (local.get $a) (i32.const 1))))
                   a))
           42)
(test-call "43.0" (lambda (a)
                    (%inline-wasm
                     '(func (param $a f64) (result f64)
                            (f64.add (local.get $a) (f64.const 1.0)))
                     a))
           42)
(test-call "43.0" (lambda (a)
                    (%inline-wasm
                     '(func (param $a f64) (result f64)
                            (f64.add (local.get $a) (f64.const 1.0)))
                     a))
           42.0)
(test-call "43.0" (lambda (a)
                    (%inline-wasm
                     '(func (param $a f32) (result f64)
                            (f64.promote_f32 (f32.add (local.get $a) (f32.const 1.0))))
                     a))
           42)
(test-call "7" (lambda (a)
                 (%inline-wasm
                  '(func (param $a (ref string)) (result i64)
                         (i64.extend_i32_u
                          (stringview_iter.advance (string.as_iter (local.get $a))
                                                   (i32.const -1))))
                  a))
           "hey hey")

(test-end* "test-inline-wasm")
