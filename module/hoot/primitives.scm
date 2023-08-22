;;; Hoot primitives
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
;;; This file exists only to be a place to define implementation-detail
;;; primitives for use by the Hoot standard library.
;;;
;;; Code:

(define-module (hoot primitives)
  #:export ()
  ;; Mark as non-declarative, as we should not have inlinable exports.
  #:declarative? #f)

;; Primitives go here...
