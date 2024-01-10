;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
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
;;; Toolchain round trip tests.
;;;
;;; Code:

(use-modules (hoot compile)
             (hoot reflect)
             (ice-9 binary-ports)
             (wasm assemble)
             (wasm parse)
             (wasm resolve)
             (wasm wat)
             (srfi srfi-64)
             (test utils))

(test-begin "test-round-trip")

;; Test that we can compile Scheme code, build a binary, disassemble
;; it all the way back to WAT form, rebuild it, and have a working
;; equivalent program.
(test-equal "round trip"
  42
  (hoot-load                            ; call start function
   (hoot-instantiate                    ; load wasm in vm
    (resolve-wasm                       ; lower again
     (wat->wasm                         ; convert back to wasm
      (wasm->wat                        ; convert to wat
       (unresolve-wasm                  ; lift int ids to names, etc.
        (call-with-input-bytevector     ; parse binary
         (assemble-wasm                 ; build binary
          (compile 42 #:debug? #t))     ; compile Scheme
         parse-wasm))))))))

(test-end* "test-round-trip")
