;;; Pass to lower and resolve wasm
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
;;;
;;; Code:

(define-module (wasm lower)
  #:use-module (wasm lower-globals)
  #:use-module (wasm lower-stringrefs)
  #:use-module (wasm resolve)
  #:export (lower-wasm stringref-lowering))

(define stringref-lowering (make-parameter 'stringref))

(define* (lower-wasm wasm #:key (stringref-lowering (stringref-lowering)))
  (resolve-wasm
   (lower-globals
    (lower-stringrefs wasm #:strategy stringref-lowering))))
