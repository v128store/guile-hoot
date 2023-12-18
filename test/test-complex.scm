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
;;; Complex number tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-complex")

(test-call "42" (lambda (x y) (make-rectangular x y)) 42 0)
(test-call "1.0+2.0i" (lambda (x y) (make-rectangular x y)) 1 2)

(test-call "0" (lambda (x y) (make-polar x y)) 0 3)
(test-call "42" (lambda (x y) (make-polar x y)) 42 0)
(test-call "+nan.0+nan.0i" (lambda (x y) (make-polar x y)) +inf.0 +inf.0)
(test-call "4.322418446945118+6.731767878463172i" (lambda (x y) (make-polar x y)) 8.0 1.0)

(test-end* "test-complex")
