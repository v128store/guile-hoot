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
;;; Keyword tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-keywords")

(test-call "foo" keyword->symbol #:foo)
(test-call "#:foo" symbol->keyword 'foo)
(test-call "#t" eq? #:foo #:foo)
(test-call "#f" eq? #:foo #:bar)
(test-call "#t" (lambda (f) (eq? (f 'foo) #:foo)) symbol->keyword)

(test-end* "test-keywords")
