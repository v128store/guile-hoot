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
;;; Atomic box tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-atomics")

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

(test-end* "test-atomics")
