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
;;; Fluid tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-fluids")

(test-call "(42 (69 100) 42)"
           (lambda (f)
             (let* ((fluid (make-fluid 42))
                    (a (fluid-ref fluid))
                    (b (with-fluid* fluid 69 (lambda () (f fluid))))
                    (c (fluid-ref fluid)))
               (list a b c)))
           (lambda (fluid)
             (let ((v (fluid-ref fluid)))
               (fluid-set! fluid 100)
               (list v (fluid-ref fluid)))))

(test-end* "test-fluids")
