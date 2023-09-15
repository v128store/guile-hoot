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
;;; Prompt tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-prompts")

(test-call "42" (lambda (f tag)
                  (call-with-prompt tag
                    (lambda () (f))
                    (lambda (k) #f)))
                (lambda () 42)
                "hey")

(test-call "69" (lambda (f tag)
                  (call-with-prompt tag
                    (lambda () (1+ (f tag)))
                    (lambda (k v) v)))
                (lambda (tag) (abort-to-prompt tag 69))
                "hey")

(test-call "69"
           (lambda (abort-to-prompt tag)
             (call-with-prompt tag
                               (lambda ()
                                 (dynamic-wind values
                                     (lambda () 42)
                                     (lambda () (abort-to-prompt tag 69))))
                               (lambda (k v) v)))
           abort-to-prompt "hey")
(test-call "69"
           (lambda (abort-to-prompt tag)
             (call-with-prompt tag
                               (lambda ()
                                 (dynamic-wind values
                                     (lambda () (abort-to-prompt tag 42))
                                     (lambda () (abort-to-prompt tag 69))))
                               (lambda (k v) v)))
           abort-to-prompt "hey")

(test-call "69" (lambda (f tag)
                  (call-with-prompt tag
                    (lambda () (- (f tag)))
                    (lambda (k v) (k (- -2 v)))))
                (lambda (tag) (abort-to-prompt tag 67))
                "hey")

(test-call "4"
           (lambda ()
             (% (+ 1 (call/cc (lambda (k) (+ 2 (k 3))))))))

(test-end* "test-prompts")
