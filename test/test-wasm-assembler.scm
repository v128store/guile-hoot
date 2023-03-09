;;; WebAssembly binary parser
;;; Copyright (C) 2023 Igalia, S.L.
;;;
;;; This library is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Parser for WebAssembly binary format
;;;
;;; Code:

(use-modules (wasm assemble)
             (wasm parse)
             (ice-9 binary-ports)
             (srfi srfi-64))

(test-begin "test-wasm-assembler")

(define-syntax-rule (test-wat->wasm expected wat)
  (begin
    (test-equal expected (wat->wasm 'wat))
    (test-equal expected
                (assemble-wasm (call-with-input-bytevector expected parse-wasm)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 0 1 127 3 2 1 0 10 6 1 4 0 65 42 11)
 (module
  (func (param) (result i32)
        (i32.const 42))))

(define basic-types.wasm
  (call-with-input-file "./basic-types.wasm" get-bytevector-all))
(define basic-types/1
  (call-with-input-bytevector basic-types.wasm parse-wasm))
(define basic-types.wasm/2
  (assemble-wasm basic-types/1))
(define basic-types/2
  (call-with-input-bytevector basic-types.wasm/2 parse-wasm))
(define basic-types.wasm/3
  (assemble-wasm basic-types/2))

;; For wasm files like basic-types.wasm that are produced by external
;; tools, we don't aim for byte-for-byte parsing and re-serialization,
;; notably because binaryen emits deprecated GC opcodes which we rewrite
;; to the updated ones.  But if we parse a file that we generate and
;; then re-serialize it, they should be the same.
(test-equal "basic types reassembly"
            basic-types.wasm/2 basic-types.wasm/3)

(test-end "test-wasm-assembler")

