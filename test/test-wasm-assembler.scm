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

;; Test type resolution for functions with similar signatures
(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 6 1 96 1 127 1 127 3 3 2 0 0 10 11 2 4 0 32
        0 11 4 0 32 0 11)
 (module
  (func $a (param $x i32) (result i32)
        (local.get $x))
  (func $b (param $y i32) (result i32)
        (local.get $y))))

;; Test vector instructions
(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 0 1 123 3 2 1 0 10 8 1 6 0 65 23 253
        15 11)
 (module (func (result v128) (i8x16.splat (i32.const 23)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 0 1 123 3 2 1 0 10 8 1 6 0 65 23 253
        16 11)
 (module (func (result v128) (i16x8.splat (i32.const 23)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 0 1 123 3 2 1 0 10 8 1 6 0 65 23 253
        17 11)
 (module (func (result v128) (i32x4.splat (i32.const 23)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 0 1 123 3 2 1 0 10 8 1 6 0 66 23 253
        18 11)
 (module (func (result v128) (i64x2.splat (i64.const 23)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 0 1 123 3 2 1 0 10 11 1 9 0 67 208
        15 73 64 253 19 11)
 (module (func (result v128) (f32x4.splat (f32.const 3.14159)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 0 1 123 3 2 1 0 10 15 1 13 0 68 110
        134 27 240 249 33 9 64 253 20 11)
 (module (func (result v128) (f64x2.splat (f64.const 3.14159)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 4 1 94 127 0)
 (module (type $foo (array i32))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 4 1 94 127 1)
 (module (type $foo (array (mut i32)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 7 1 95 2 127 0 127 0)
 (module (type $foo (struct (field $foo i32) (field $bar i32)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 7 1 95 2 127 1 127 0)
 (module (type $foo (struct (field $a (mut i32)) (field $b i32)))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 7 1 95 2 127 0 127 1)
 (module (type $foo (struct (field $a i32) (field $b (mut i32))))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 7 1 95 2 127 1 127 1)
 (module (type $foo (struct (field $a (mut i32)) (field $b (mut i32))))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 9 1 79 1 80 0 95 1 127 1)
 (module
  (rec
   (type $heap-object
         (struct (field $hash (mut i32)))))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 19 1 79 2 80 0 95 1 127 1 80 1 0 95 2 127 1
        107 111 0)
 (module
  (rec
   (type $heap-object
         (struct (field $hash (mut i32))))
   (type $extern-ref
         (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $val (ref extern))))))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 35 1 79 3 80 0 95 1 127 1 80 1 0 95 3 127 1
        107 109 1 107 109 1 80 1 1 95 3 127 1 107 109 1 107 109 1)
 (module
  (rec
   (type $heap-object
         (struct (field $hash (mut i32))))
   (type $pair
         (sub $heap-object
              (struct
               (field $hash (mut i32))
               (field $car (mut (ref eq)))
               (field $cdr (mut (ref eq))))))
   (type $mutable-pair
         (sub $pair
              (struct
               (field $hash (mut i32))
               (field $car (mut (ref eq)))
               (field $cdr (mut (ref eq)))))))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 4 1 96 0 0 3 2 1 0 10 9 1 7 0 65 0 14 0 0 11)
 (module (func (i32.const 0) (br_table 0))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 1 127 0 3 2 1 0 10 20 1 18 0 2 64 2
        64 2 64 32 0 14 2 2 1 0 11 11 11 11)
 (module
  (func (param $i i32)
    (block $l1
      (block $l2
        (block $l3
          (br_table $l1 $l2 $l3 (local.get $i))))))))

(test-wat->wasm
 #vu8(0 97 115 109 1 0 0 0 1 5 1 96 0 1 127 3 2 1 0 10 14 1 12 0 65 0 4
        127 65 1 5 65 2 11 11)
 '(module
   (func (result i32)
         (if i32
             (i32.const 0)
             (then (i32.const 1))
             (else (i32.const 2))))))

(when (and (batch-mode?) (not (test-passed?)))
  (exit 1))

(test-end "test-wasm-assembler")

