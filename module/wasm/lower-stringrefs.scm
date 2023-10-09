;;; WebAssembly assembler
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
;;; The hoot compiler and run-time use stringref to represent strings,
;;; albeit wrapped in a struct so they can have a hash code.  Stringref
;;; (https://github.com/WebAssembly/stringref/blob/main/proposals/stringref/Overview.md)
;;; is an experimental extension to WebAssembly which exposes "host
;;; strings": strings whose representation is opaque and managed by the
;;; host.  On a web browser, these will likely be WTF-16 strings; on
;;; anything else, it's probably WTF-8, though for the Hoot VM used in
;;; development, we use Guile strings there.
;;;
;;; Stringref does not yet have consensus among the WebAssembly
;;; standardization group.  Chrome has an experimental
;;; --experimental-wasm-stringref flag that users have to enable
;;; explicitly, and which is also on for a limited time and set of web
;;; sites for an "origin trial".  But, it's not shipping by default
;;; anywhere, so we need a mitigation.
;;;
;;; A competitor to stringref is JavaScript String Builtins
;;; (https://github.com/WebAssembly/js-string-builtins/blob/main/proposals/js-string-builtins/Overview.md).
;;; This proposal instead ships a new set of functions for JavaScript
;;; hosts, off of a new WebAssembly.String object:
;;;
;;;  - WebAssembly.String.fromWtf16Array(array, start, end) -> str
;;;  - WebAssembly.String.toWtf16Array(str, array, start) -> length
;;;  - WebAssembly.String.fromWtf8Array(array, start, end) -> str
;;;  - WebAssembly.String.fromCharCode(u16) -> str
;;;  - WebAssembly.String.fromCodePoint(u32) -> str
;;;  - WebAssembly.String.codePointAt(idx) -> i32
;;;  - WebAssembly.String.charCodeAt(idx) -> i32
;;;  - WebAssembly.String.length(str) -> i32
;;;  - WebAssembly.String.concat(a, b) -> str
;;;  - WebAssembly.String.substring(str, start, end) -> str
;;;  - WebAssembly.String.equals(a, b) -> i32
;;;  - WebAssembly.String.compare(a, b) -> i32
;;;
;;; The intent is that if a WebAssembly module imports one of these
;;; functions, it can recognize it specially, as is done for
;;; e.g. Math.cos, and compile it appropriately.  Currently we would use
;;; externref to represent these strings, but in future there could be
;;; more precise typing.
;;;
;;; Another option would be to just use what WebAssembly gives us, which
;;; is arrays and structs.  That would work fine within the module, but
;;; it is pretty annoying from the host side: for every string value
;;; that crosses the threshold, you need to convert to or from an i8
;;; array.  However, JS hosts cannot access the i8 array, so you would
;;; need to use a reflection module and go byte-by-byte.  Very annoying.
;;;
;;; Finally there is a mix between the i8 array and string builtins
;;; approach: use wtf8 internally, but then externalize via calling an
;;; import, on the boundary, from the WebAssembly side of things.  It
;;; would work for function arguments with stringref-typed arguments and
;;; results.  Nested data would still need to be picked apart by the
;;; host using a reflection module.
;;;
;;; In summary, our options are:
;;;
;;;  1. Just stringref.  Works for Hoot VM, and Chrome with an origin
;;;     trial.
;;;
;;;  2. Just i8 arrays.  Works everywhere but is expensive on the
;;;     boundary.  Strings are (array i8), immutable, and (array i8) on
;;;     boundary.  Iteration views are a struct with an offset.
;;;     string.const loads an immutable i8 array global.  We have a
;;;     little state machine for advancing / decoding.  Encode and
;;;     concat is array.copy.
;;;
;;;  3. Just i8 arrays, but for external functions with string params or
;;;     results, the generated wasm calls
;;;     WebAssembly.String.fromWtf8Array as an import, and types those
;;;     values as externref.  Same with toWtf8Array in the other
;;;     direction, except that doesn't exist yet.
;;;
;;;  4. Strings are externref.  string.const loads an immutable i8 array
;;;     global, but then eagerly calls fromWtf8Array on it.  All
;;;     operations proxy through the JS builtin functions.  We still
;;;     have a struct iterator.
;;;
;;; This module implements a lowering strategy for (3).  If the host
;;; does not support WebAssembly.String, we polyfill the import.  This
;;; way we can target any browser, and take advantage of fromWtf8Array
;;; on the host side if present.  We don't currently think that (4) will
;;; have good enough performance, and in any case the builtin strings
;;; proposal doesn't have the facilities we need (e.g. measure_wtf8).
;;;
;;; Code:

(define-module (wasm lower-stringrefs)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (append-map filter-map))
  #:use-module (rnrs bytevectors)
  #:use-module (wasm link)
  #:use-module (wasm types)
  #:use-module (wasm wat)
  #:export (lower-stringrefs))


(define (wtf8-stdlib)
  ;; Generalized UTF-8 decoder is a translation of:
  ;; https://chromium.googlesource.com/v8/v8/+/main/src/third_party/utf8-decoder/generalized-utf8-decoder.h

  ;; generalized utf-8 decoder states
  (define %wtf8-reject 0)
  (define %wtf8-accept 11)
  (define %wtf8-two-byte 22)
  (define %wtf8-three-byte 33)
  (define %wtf8-four-byte 44)
  (define %wtf8-four-byte-low 55)
  (define %wtf8-three-byte-high 66)
  (define %wtf8-four-byte-mid-high 77)

  ;; This first table maps bytes to character to a transition.
  ;;
  ;; The transition value takes a state to a new state, but it also determines
  ;; the set of bits from the current byte that contribute to the decoded
  ;; codepoint:
  ;;
  ;;   Transition | Current byte bits that contribute to decoded codepoint
  ;;   -------------------------------------------------------------------
  ;;    0, 1      | #b01111111
  ;;    2, 3      | #b00111111
  ;;    4, 5      | #b00011111
  ;;    6, 7      | #b00001111
  ;;    8, 9      | #b00000111
  ;;    10        | #b00000011
  ;;
  ;; Given the WTF-8 encoding, we therefore have the following constraints:
  ;;
  ;;   1. The transition value for 1-byte encodings should have the value 0 or
  ;;      1 so that we preserve all of the low 7 bits.
  ;;   2. Continuation bytes (#x80 to #xBF) are of the form #b10xxxxxx, and
  ;;      therefore should have transition value between 0 and 3.
  ;;   3. Leading bytes for 2-byte encodings are of the form #b110yyyyy, and
  ;;      therefore the transition value can be between 2 and 5.
  ;;   4. Leading bytes for 3-byte encodings (#b1110zzzz) need transition
  ;;      value between 4 and 7.
  ;;   5. Leading bytes for 4-byte encodings (#b11110uuu) need transition
  ;;      value between 6 and 9.
  ;;   6. We need more states to impose irregular constraints.  Sometimes we
  ;;      can use the knowldege that e.g. some high significant bits of the
  ;;      xxxx in #b1110xxxx are 0, then we can use a higher transition value.
  ;;   7. Transitions to invalid states can use any transition value.
  (define %wtf8-transitions
    #vu8(0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 00-0F
         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 10-1F
         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 20-2F
         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 30-3F
         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 40-4F
         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 50-5F
         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 60-6F
         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0   ;; 70-7F
         1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1   ;; 80-8F
         2  2  2  2  2  2  2  2  2  2  2  2  2  2  2  2   ;; 90-9F
         3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3   ;; A0-AF
         3  3  3  3  3  3  3  3  3  3  3  3  3  3  3  3   ;; B0-BF
         8  8  4  4  4  4  4  4  4  4  4  4  4  4  4  4   ;; C0-CF
         4  4  4  4  4  4  4  4  4  4  4  4  4  4  4  4   ;; D0-DF
         9  5  5  5  5  5  5  5  5  5  5  5  5  5  5  5   ;; E0-EF
         10 6  6  6  7  8  8  8  8  8  8  8  8  8  8  8)) ;; F0-FF

  ;; This second table maps a state to a new state when adding a transition.
  ;;     00-7F
  ;;     |  80-8F
  ;;     |  |  90-9F
  ;;     |  |  |  A0-BF
  ;;     |  |  |  |  C2-DF
  ;;     |  |  |  |  |  E1-EF
  ;;     |  |  |  |  |  |  F1-F3
  ;;     |  |  |  |  |  |  |  F4
  ;;     |  |  |  |  |  |  |  |  C0, C1, F5-FF
  ;;     |  |  |  |  |  |  |  |  |  E0
  ;;     |  |  |  |  |  |  |  |  |  |  F0
  (define %wtf8-states
    #vu8(0  0  0  0  0  0  0  0  0  0  0   ;; REJECT = 0
         11 0  0  0  22 33 44 55 0  66 77  ;; ACCEPT = 11
         0  11 11 11 0  0  0  0  0  0  0   ;; 2-byte = 22
         0  22 22 22 0  0  0  0  0  0  0   ;; 3-byte = 33
         0  33 33 33 0  0  0  0  0  0  0   ;; 4-byte = 44
         0  33 0  0  0  0  0  0  0  0  0   ;; 4-byte low = 55
         0  0  0  22 0  0  0  0  0  0  0   ;; 3-byte high = 66
         0  0  33 33 0  0  0  0  0  0  0)) ;; 4-byte mid/high = 77

  (wat->wasm
   `((type $wtf8 (array (mut i8)))
     (type $stringview-iter
           (struct (field $wtf8 (ref $wtf8))
                   (field $byte-offset (mut i32))
                   (field $codepoint-offset (mut i32))))

     (func $wtf8->extern-string (import "rt" "wtf8_to_string")
           (param $wtf8 (ref null $wtf8))
           (result (ref extern)))
     (func $extern-string->wtf8 (import "rt" "string_to_wtf8")
           (param $str (ref null extern))
           (result (ref $wtf8)))

     (type $immutable-bytes (array i8))
     (data $wtf8-transitions ,%wtf8-transitions)
     (data $wtf8-states ,%wtf8-states)
     (global $wtf8-transitions (ref $immutable-bytes)
             (array.new_data $immutable-bytes $wtf8-transitions
                             (i32.const 0)
                             (i32.const
                              ,(bytevector-length %wtf8-transitions))))
     (global $wtf8-states (ref $immutable-bytes)
             (array.new_data $immutable-bytes $wtf8-states
                             (i32.const 0)
                             (i32.const
                              ,(bytevector-length %wtf8-states))))

     (func $decode-wtf8 (param $byte i32) (param $buf i32) (param $state i32)
           (result i32 i32) ; codepoint, state
           (local $type i32)
           (local.set $type
                      (array.get_u $immutable-bytes
                                   (global.get $wtf8-transitions)
                                   (local.get $byte)))
           ;; Two values: first push the codepoint
           (i32.or (i32.shl (local.get $buf) (i32.const 6))
                   (i32.and (local.get $byte)
                            (i32.shr_u (i32.const #x7f)
                                       (i32.shr_u (local.get $type)
                                                  (i32.const 1)))))
           ;; Then the state
           (array.get_u $immutable-bytes
                        (global.get $wtf8-states)
                        (i32.add (local.get $state) (local.get $type))))

     ;; Downside of wtf-8: byte-by-byte comparison.
     (func $string.compare
           (param $a (ref $wtf8))
           (param $b (ref $wtf8))
           (result i32)
           (local $i i32)
           (local $d i32)
           (if (ref.eq (local.get $a) (local.get $b))
               (then (return (i32.const 0))))
           (loop $lp
             i32
             (if (i32.eq (array.len (local.get $a)) (local.get $i))
                 (then
                  (if (i32.eq (array.len (local.get $b)) (local.get $i))
                      (then (return (i32.const 0)))
                      (else (return (i32.const -1))))))
             (if (i32.eq (array.len (local.get $b)) (local.get $i))
                 (then (return (i32.const 1))))
             (local.tee
              $d
              (i32.sub
               (array.get_u $wtf8 (local.get $a) (local.get $i))
               (array.get_u $wtf8 (local.get $b) (local.get $i))))
             (if (i32.eqz)
                 (then
                  (local.set $i
                             (i32.add (local.get $i) (i32.const 1)))
                  (br $lp)))
             (if i32
                 (i32.lt_s (local.get $d) (i32.const 0))
                 (then (i32.const -1))
                 (else (i32.const 0)))))

     (func $string.eq (param $a (ref $wtf8)) (param $b (ref $wtf8))
           (result i32)
           (i32.eqz (call $string.compare (local.get $a) (local.get $b))))

     (func $string.as_iter (param $wtf8 (ref $wtf8))
           (result (ref $stringview-iter))
           (struct.new $stringview-iter
                       (local.get $wtf8)
                       (i32.const 0)
                       (i32.const 0)))
     
     (func $stringview_iter.next
           (param $iter (ref $stringview-iter))
           (result i32)
           (local $wtf8 (ref $wtf8))
           (local $cp i32)
           (local $state i32)
           (local $i i32)
           (local.set $wtf8 (struct.get $stringview-iter $wtf8
                                        (local.get $iter)))
           (local.set $i (struct.get $stringview-iter $byte-offset
                                     (local.get $iter)))
           (local.set $state (i32.const ,%wtf8-accept))
           (if (i32.ge_u (local.get $i) (array.len (local.get $wtf8)))
               (then (return (i32.const -1))))
           (loop $lp
             (if (i32.ge_u (local.get $i) (array.len (local.get $wtf8)))
                 ;; Bad WTF-8.
                 (then (unreachable)))
             (call $decode-wtf8
                   (array.get_u $wtf8 (local.get $wtf8) (local.get $i))
                   (local.get $cp)
                   (local.get $state))
             (local.set $state)
             (local.set $cp)
             ;; Must be valid WTF-8!
             (if (i32.eq (local.get $state) (i32.const ,%wtf8-reject))
                 (then (unreachable)))
             (local.set $i (i32.add (local.get $i) (i32.const 1)))
             (if (i32.ne (local.get $state) (i32.const ,%wtf8-accept))
                 (then (br $lp))))
           (struct.set $stringview-iter $byte-offset
                       (local.get $iter) (local.get $i))
           (struct.set $stringview-iter $codepoint-offset
                       (local.get $iter)
                       (i32.add (struct.get $stringview-iter
                                            $codepoint-offset
                                            (local.get $iter))
                                (i32.const 1)))
           (local.get $cp))

     (func $stringview_iter.advance
           (param $iter (ref $stringview-iter)) (param $count i32)
           (result i32)
           (local $wtf8 (ref $wtf8))
           (local $state i32)
           (local $i i32)
           (local $advanced i32)
           (local.set $wtf8 (struct.get $stringview-iter $wtf8
                                        (local.get $iter)))
           (local.set $i (struct.get $stringview-iter $byte-offset
                                     (local.get $iter)))
           (local.set $state (i32.const ,%wtf8-accept))
           (if (i32.eqz (local.get $count))
               (then (return (i32.const 0))))
           (loop $lp
             (if (i32.lt_u (local.get $i) (array.len (local.get $wtf8)))
                 (then
                  (call $decode-wtf8
                        (array.get_u $wtf8 (local.get $wtf8) (local.get $i))
                        (i32.const 0)
                        (local.get $state))
                  (local.set $state)
                  (drop)
                  ;; Must be valid WTF-8!
                  (if (i32.eq (local.get $state) (i32.const ,%wtf8-reject))
                      (then (unreachable)))
                  (local.set $i (i32.add (local.get $i) (i32.const 1)))
                  (if (i32.eq (local.get $state (i32.const ,%wtf8-accept)))
                      (then
                       (local.set $advanced
                                  (i32.add (local.get $advanced)
                                           (i32.const 1)))
                       (if (i32.lt_u (local.get $advanced)
                                     (local.get $count))
                           (then (br $lp))))
                      (else
                       (br $lp))))
                 (else
                  ;; Must be valid WTF-8!
                  (if (i32.ne (local.get $state) (i32.const ,%wtf8-accept))
                      (then (unreachable))))))
           (struct.set $stringview-iter $byte-offset
                       (local.get $iter) (local.get $i))
           (struct.set $stringview-iter $codepoint-offset
                       (local.get $iter)
                       (i32.add (struct.get $stringview-iter
                                            $codepoint-offset
                                            (local.get $iter))
                                (local.get $advanced)))
           (local.get $advanced))

     (func $stringview_iter.slice
           (param $iter (ref $stringview-iter)) (param $count i32)
           (result (ref $wtf8))
           (local $wtf8 (ref $wtf8))
           (local $start i32)
           (local $len i32)
           (local $temp (ref $stringview-iter))
           (local $out (ref $wtf8))
           (local.set $wtf8 (struct.get $stringview-iter $wtf8
                                        (local.get $iter)))
           (local.set $start (struct.get $stringview-iter $byte-offset
                                         (local.get $iter)))
           (local.set $temp (struct.new $stringview-iter
                                        (local.get $wtf8)
                                        (local.get $start)
                                        (struct.get $stringview-iter
                                                    $codepoint-offset
                                                    (local.get $iter))))
           (call $stringview_iter.advance (local.get $temp)
                 (local.get $count))
           (drop)
           (local.set $len
                      (i32.sub (struct.get $stringview-iter $byte-offset
                                           (local.get $iter))
                               (local.get $start)))
           (local.set $out (array.new_default $wtf8 (local.get $len)))
           (array.copy $wtf8 $wtf8
                       (local.get $out) (i32.const 0)
                       (local.get $wtf8) (local.get $start) (local.get $len))
           (local.get $out))

     (func $string.encode_wtf8_array
           (param $wtf8 (ref $wtf8)) (param $out (ref $wtf8)) (param $pos i32)
           (array.copy $wtf8 $wtf8
                       (local.get $out)
                       (local.get $pos)
                       (local.get $wtf8)
                       (i32.const 0)
                       (array.len (local.get $wtf8))))

     (func $string.new_lossy_utf8_array
           (param $buf (ref $wtf8)) (param $start i32) (param $len i32)
           (result (ref $wtf8))
           (local $out (ref $wtf8))
           ;; FIXME: validate buffer as wtf8
           (local.set $out (array.new_default $wtf8 (local.get $len)))
           (array.copy $wtf8 $wtf8
                       (local.get $out) (i32.const 0)
                       (local.get $buf) (local.get $start) (local.get $len))
           (local.get $out))

     (func $string.measure_wtf16 (param $wtf8 (ref $wtf8)) (result i32)
           (local $iter (ref $stringview-iter))
           (local $cp i32)
           (local $count i32)
           (local.set $iter (call $string.as_iter (local.get $wtf8)))
           (loop $lp
             (local.set $cp (call $stringview_iter.next (local.get $iter)))
             (if (i32.le_s (i32.const 0) (local.get $cp))
                 (then
                  (local.set $count
                             (i32.add (i32.add (local.get $count)
                                               (i32.const 1))
                                      (i32.gt_u (local.get $cp)
                                                (i32.const #xffff))))
                  (br $lp))))
           (local.get $count)))))

(define (lower-stringrefs/wtf8 wasm)
  (define make-id
    (let ((counter 0))
      (lambda (stem)
        (let ((sym (string->symbol (format #f "~a-~a" stem counter))))
          (set! counter (1+ counter))
          sym))))

  (define %strings (make-hash-table))
  (define (intern-string! str)
    (or (hash-ref %strings str)
        (let ((id (make-id '$stringref)))
          (hash-set! %strings str id)
          id)))

  (match wasm
    (($ <wasm> types imports funcs tables memories globals exports start
        elems datas tags () custom)
     ;; need to replace string-typed imports with wrappers
     (define (visit-heap-type type)
       (match type
         ('string '$wtf8)
         ('stringview_wtf8 (error "lowering wtf8 views unsupported"))
         ('stringview_wtf16 (error "lowering wtf16 views unsupported"))
         ('stringview_iter '$stringview-iter)
         (_ type)))
     (define (visit-val-type type)
       (match type
         (($ <ref-type> nullable? ht)
          (make-ref-type nullable? (visit-heap-type ht)))
         (_ type)))
     (define (visit-ref-type type)
       (visit-val-type type))
     (define (visit-param param)
       (match param
         (($ <param> id type)
          (make-param id (visit-val-type type)))))
     (define (visit-field field)
       (match field
         (($ <field> id mutable? type)
          (make-field id mutable? (visit-val-type type)))))
     (define (visit-func-sig type)
       (match type
         (($ <func-sig> params results)
          (make-func-sig (map visit-param params)
                         (map visit-val-type results)))))
     (define (visit-base-type type)
       (match type
         (($ <struct-type> fields)
          (make-struct-type (map visit-field fields)))
         (($ <array-type> mutable? type)
          (make-array-type mutable? (visit-val-type type)))
         (_
          (visit-func-sig type))))
     (define (visit-sub-type type)
       (match type
         (($ <sub-type> final? supers type)
          (make-sub-type final? supers (visit-base-type type)))
         (_ (visit-base-type type))))
     (define (visit-type-use type)
       (match type
         (($ <type-use> id sig)
          (make-type-use id (visit-func-sig sig)))))
     (define (visit-table-type type)
       (match type
         (($ <table-type> limits elem-type)
          (make-table-type limits (visit-val-type elem-type)))))
     (define (visit-global-type type)
       (match type
         (($ <global-type> mutable? type)
          (make-global-type mutable? (visit-val-type type)))))
     (define (visit-block-type type)
       (match type
         (#f #f)
         (($ <type-use>) (visit-type-use type))
         ((or ($ <ref-type>) (? symbol?)) (visit-val-type type))))
     (define visit-inst
       (match-lambda
        (((and inst (or 'block 'loop)) label type body)
         `(,inst ,label ,(visit-block-type type)
                 ,(visit-expr body)))
        (('if label type consequent alternate)
         `(if ,label ,(visit-block-type type)
              ,(visit-expr consequent)
              ,(visit-expr alternate)))
        (('try label type body catches catch-all)
         `(try ,label ,(visit-block-type type)
               ,(visit-expr body)
               ,(map visit-expr catches)
               ,(and=> catch-all visit-expr)))
        (('try_delegate label type body handler)
         `(try_delegate ,label ,(visit-block-type type)
                        ,(visit-expr body)
                        ,handler))
        (('call_indirect table type)
         `(call_indirect ,table ,(visit-type-use type)))
        (('select types) `(select ,(map visit-val-type types)))

        ;; GC instructions.
        (('ref.null ht) `(ref.null ,(visit-heap-type ht)))
        (((and inst (or 'ref.test 'ref.cast)) rt)
         `(,inst ,(visit-ref-type rt)))
        (((and inst (or 'br_on_cast 'br_on_cast_fail)) label rt1 rt2)
         `(,inst ,label ,(visit-ref-type rt1) ,(visit-ref-type rt2)))

        ;; Stringref instructions.
        (('string.const str)             `(global.get ,(intern-string! str)))
        (('string.new_utf8 mem)          `(call ,(symbol-append
                                                  '$string.new_utf8_ mem)))
        (('string.new_lossy_utf8 mem)    `(call ,(symbol-append
                                                  '$string.new_lossy_utf8_ mem)))
        (('string.new_wtf8 mem)          `(call ,(symbol-append
                                                  '$string.new_wtf8_ mem)))
        (('string.new_wtf16 mem)         `(call ,(symbol-append
                                                  '$string.new_wtf16_ mem)))
        (('string.measure_wtf8)          '(array.len))
        (('string.measure_utf8)          '(call $string.measure_utf8))
        (('string.measure_wtf16)         '(call $string.measure_wtf16))
        (('string.encode_utf8 mem)       `(call ,(symbol-append
                                                '$string.encode_utf8_ mem)))
        (('string.encode_lossy_utf8 mem) `(call ,(symbol-append
                                                  '$string.encode_lossy_utf8_ mem)))
        (('string.encode_wtf8 mem)       `(call ,(symbol-append '$string.encode_wtf8_ mem)))
        (('string.encode_wtf16 mem)      `(call ,(symbol-append
                                                  '$string.encode_wtf16_ mem)))
        (('string.concat)                '(call $string.concat))
        (('string.eq)                    '(call $string.eq))
        (('string.is_usv_sequence)       '(call $string.is_usv_sequence))
        (('string.compare)               '(call $string.compare))
        (('string.from_code_point)       '(call $string.from_code_point))

        (('string.as_wtf8)               '(call $string.as_wtf8))
        (('stringview_wtf8.advance)      '(call $stringview_wtf8.advance))
        (('stringview_wtf8.encode_utf8 mem)
                                         `(call ,(symbol-append
                                                  '$stringview_wtf8.encode_utf8_ mem)))
        (('stringview_wtf8.encode_lossy_utf8 mem)
                                         `(call ,(symbol-append
                                                  '$stringview_wtf8.encode_lossy_utf8_ mem)))
        (('stringview_wtf8.encode_wtf8 mem)
                                         `(call ,(symbol-append
                                                  '$stringview_wtf8.encode_wtf8_ mem)))
        (('stringview_wtf8.slice)        '(call $stringview_wtf8.slice))

        (('string.as_wtf16)              '(call $string.as_wtf16))
        (('stringview_wtf16.length)      '(call $stringview_wtf16.length))
        (('stringview_wtf16.get_codeunit) `(call $stringview_wtf16.get_codeunit))
        (('stringview_wtf16.encode mem)  `(call ,(symbol-append
                                                  '$stringview_wtf16.encode_ mem)))
        (('stringview_wtf16.slice)       '(call $stringview_wtf16.slice))

        (('string.as_iter)               '(call $string.as_iter))
        (('stringview_iter.next)         '(call $stringview_iter.next))
        (('stringview_iter.advance)      '(call $stringview_iter.advance))
        (('stringview_iter.rewind)       '(call $stringview_iter.rewind))
        (('stringview_iter.slice)        '(call $stringview_iter.slice))

        (('string.new_utf8_array)        '(call $string.new_utf8_array))
        (('string.new_lossy_utf8_array)  '(call $string.new_lossy_utf8_array))
        (('string.new_wtf8_array)        '(call $string.new_wtf8_array))
        (('string.new_wtf16_array)       '(call $string.new_wtf16_array))
        (('string.encode_utf8_array)     '(call $string.encode_utf8_array))
        (('string.encode_lossy_utf8_array) '(call $string.encode_lossy_utf8_array))
        (('string.encode_wtf8_array)     '(call $string.encode_wtf8_array))
        (('string.encode_wtf16_array)    '(call $string.encode_wtf16_array))

        (inst inst)))
     (define (visit-expr expr)
       (map visit-inst expr))
     (define (visit-init expr)
       (visit-expr expr))
     (define (visit-func func)
       (define visit-local
         (match-lambda
          (($ <local> id type)
           (make-local id (visit-val-type type)))))
       (match func
         (($ <func> id type locals body)
          (let ((type (visit-type-use type))
                (locals (map visit-local locals))
                (body (visit-expr body)))
            (make-func id type locals body)))))

     (define (lower-extern-val-type type)
       (match type
         (($ <ref-type> nullable? 'string)
          (make-ref-type nullable? 'extern))
         (($ <ref-type> nullable? (or 'stringview_wtf8
                                      'stringview_wtf16
                                      'stringview_iter))
          (error "import param/result with stringview type unimplemented" type))
         (_ type)))
     (define (lower-extern-func-param type)
       (match type
         (($ <ref-type> nullable? 'string)
          '((call $wtf8->extern-string)))
         (($ <ref-type> nullable? (or 'stringview_wtf8
                                      'stringview_wtf16
                                      'stringview_iter))
          (error "import param with stringview type unimplemented" type))
         (_ '())))
     (define (lift-extern-func-result type)
       (match type
         (($ <ref-type> nullable? 'string)
          '((call $extern-string->wtf8)))
         (($ <ref-type> nullable? (or 'stringview_wtf8
                                      'stringview_wtf16
                                      'stringview_iter))
          (error "import result with stringview type unimplemented" type))
         (_ '())))
     (define (lower-extern-func-type type)
       (match type
         (($ <type-use> tid
             ($ <func-sig> (($ <param> pid ptype) ...) (rtype ...)))
          (make-type-use tid
                         (make-func-sig
                          (map make-param pid
                               (map lower-extern-val-type ptype))
                          (map lower-extern-val-type rtype))))))
     (define (lower-extern-func id wrapped-id type)
       (match type
         (($ <type-use> _ ($ <func-sig> (($ <param> _ params) ...) results))
          (let ((param-count (length params)))
            (make-func
             id
             (visit-type-use type)
             (map (lambda (type) (make-local #f (lower-extern-val-type type)))
                  results)
             (let lp ((params params) (i 0))
               (match params
                 ((param . params)
                  `((local.get ,i)
                    ,@(lower-extern-func-param param)
                    . ,(lp params (1+ i))))
                 (()
                  `((call ,wrapped-id)
                    ,@(reverse (map (lambda (i) `(local.set ,i))
                                    (iota (length results) param-count)))
                    . ,(let lp ((results results) (i param-count))
                         (match results
                           (() '())
                           ((result . results)
                            `((local.get ,i)
                              ,@(lift-extern-func-result result)
                              . ,(lp results (1+ i)))))))))))))))

     (let ((types (map (match-lambda
                        (($ <rec-group> (($ <type> id type) ...))
                         (make-rec-group
                          (map make-type id (map visit-sub-type type))))
                        (($ <type> id type)
                         (make-type id (visit-sub-type type))))
                       types))
           (imports (map
                     (match-lambda
                       (($ <import> mod name kind id type)
                        (let* ((internal? (eqv? (string-ref name 0) #\$))
                               (type* (match kind
                                        ('func (if internal?
                                                   (visit-type-use type)
                                                   (lower-extern-func-type type)))
                                        ('table (visit-table-type type))
                                        ('memory type)
                                        ('global (visit-global-type type))))
                               (id* (and (eq? kind 'func)
                                         (not internal?)
                                         (not (equal? type type*))
                                         (make-id (symbol-append id '-stringref)))))
                          (cons (and id* (lower-extern-func id id* type))
                                (make-import mod name kind (or id* id) type*)))))
                     imports))
           (funcs (map visit-func funcs))
           (tables (map (match-lambda
                         (($ <table> id type init)
                          (make-table id (visit-table-type type)
                                      (and init (visit-init init)))))
                        tables))
           (globals (map (match-lambda
                          (($ <global> id ($ <global-type> mutable? vt) init)
                           (let* ((vt (visit-val-type vt))
                                  (type (make-global-type mutable? vt)))
                             (make-global id type (visit-init init)))))
                         globals))
           (elems (map (match-lambda
                        (($ <elem> id mode table type offset inits)
                         (make-elem id mode table
                                    (visit-val-type type)
                                    (and=> offset visit-init)
                                    (map visit-init inits))))
                       elems))
           (datas (map (match-lambda
                        (($ <data> id mode mem offset init)
                         (make-data id mode mem
                                    (and=> offset visit-init)
                                    init)))
                       datas))
           (tags (map (match-lambda
                       (($ <tag> id type)
                        (make-tag id (visit-type-use type))))
                      tags)))
       (let* ((t (make-global-type #f (make-ref-type #f '$wtf8)))
              (strings (hash-map->list
                       (lambda (str id)
                         (make-global id t
                                      `((i32.const 0)
                                        (i32.const ,(bytevector-length
                                                     (string->utf8 str)))
                                        (array.new_data $wtf8 ,id))))
                       %strings))
              (wtf8 (hash-map->list
                     (lambda (str id)
                       (make-data id 'passive #f #f (string->utf8 str)))
                     %strings)))
         (add-stdlib
          (make-wasm types
                     (map cdr imports)
                     (append (filter-map car imports) funcs)
                     tables
                     memories
                     (append strings globals)
                     exports
                     start
                     elems
                     (append wtf8 datas)
                     tags
                     '()
                     custom)
          (wtf8-stdlib)))))))

(define* (lower-stringrefs wasm #:key (strategy 'wtf8))
  (match strategy
    ('stringref wasm)
    ('wtf8 (lower-stringrefs/wtf8 wasm))
    (_ (error "unknown stringref lowering strategy" strategy))))
