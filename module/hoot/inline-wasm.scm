;;; WebAssembly inline assembly
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
;;; Compiler support for inline assembly.
;;;
;;; Code:

(define-module (hoot inline-wasm)
  #:use-module (ice-9 match)
  #:use-module ((language tree-il primitives)
                #:select (add-interesting-primitive!))
  #:use-module ((language tree-il effects)
                #:select (add-primcall-effect-analyzer!))
  #:use-module ((language tree-il compile-cps)
                #:select (define-custom-primcall-converter))
  #:use-module (language tree-il)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:use-module (wasm types)
  #:use-module (wasm wat)
  #:use-module ((hoot primitives) #:select (%inline-wasm %wasm-import))
  #:export (install-inline-wasm!))

(define (inline-wasm-effect-free? args)
  (define (effect-free-expr? expr)
    (define (effect-free-inst? inst)
      (match inst
        (((or 'block 'loop) label type body)
         (effect-free-expr? body))
        (('if label type consequent alternate)
         (and (effect-free-expr? consequent)
              (effect-free-expr? alternate)))
        (('try label type body catches catch-all)
         (and (effect-free-expr? body)
              (and-map effect-free-expr? catches)
              (or (not catch-all) (effect-free-expr? catch-all))))
        (('try_delegate label type body handler)
         (effect-free-expr? body))
        ((op . args)
         (case op
           ((nop
             br br_if br_table return drop select
             local.get local.set local.tee global.get
             table.get
             i32.load i64.load f32.load f64.load
             i32.load8_s i32.load8_u
             i32.load16_s i32.load16_u
             i64.load8_s i64.load8_u
             i64.load16_s i64.load16_u
             i64.load32_s i64.load32_u
             memory.size
             i32.const i64.const f32.const f64.const
             i32.eqz i32.eq i32.ne
             i32.lt_s i32.lt_u i32.gt_s i32.gt_u
             i32.le_s i32.le_u i32.ge_s i32.ge_u
             i64.eqz i64.eq i64.ne
             i64.lt_s i64.lt_u i64.gt_s i64.gt_u
             i64.le_s i64.le_u i64.ge_s i64.ge_u
             f32.eq f32.ne f32.lt f32.gt f32.le f32.ge
             f64.eq f64.ne f64.lt f64.gt f64.le f64.ge
             i32.clz i32.ctz i32.popcnt
             i32.add i32.sub i32.mul
             i32.div_s i32.div_u i32.rem_s i32.rem_u
             i32.and i32.or i32.xor
             i32.shl i32.shr_s i32.shr_u i32.rotl i32.rotr
             i64.clz i64.ctz i64.popcnt
             i64.add i64.sub i64.mul
             i64.div_s i64.div_u i64.rem_s i64.rem_u
             i64.and i64.or i64.xor
             i64.shl i64.shr_s i64.shr_u i64.rotl i64.rotr
             f32.abs f32.neg f32.ceil f32.floor f32.trunc f32.nearest f32.sqrt
             f32.add f32.sub f32.mul f32.div f32.min f32.max f32.copysign
             f64.abs f64.neg f64.ceil f64.floor f64.trunc f64.nearest f64.sqrt
             f64.add f64.sub f64.mul f64.div f64.min f64.max f64.copysign
             i32.wrap_i64 i32.trunc_f32_s i32.trunc_f32_u
             i32.trunc_f64_s i32.trunc_f64_u
             i64.extend_i32_s i64.extend_i32_u i64.trunc_f32_s i64.trunc_f32_u
             i64.trunc_f64_s i64.trunc_f64_u
             f32.convert_i32_s f32.convert_i32_u
             f32.convert_i64_s f32.convert_i64_u
             f32.demote_f64 f64.convert_i32_s f64.convert_i32_u
             f64.convert_i64_s f64.convert_i64_u
             f64.promote_f32
             i32.reinterpret_f32 i64.reinterpret_f64 f32.reinterpret_i32
             f64.reinterpret_i64
             i32.extend8_s i32.extend16_s
             i64.extend8_s i64.extend16_s i64.extend32_s

             ref.null ref.is_null ref.func ref.eq ref.as_non_null
             struct.new struct.new_default struct.get struct.get_s struct.get_u
             array.new array.new_default array.new_fixed array.new_data
             array.new_elem array.get array.get_s array.get_u
             array.len
             ref.test ref.cast br_on_cast br_on_cast_fail
             extern.internalize extern.externalize
             ref.i31 i31.get_s i31.get_u

             string.new_utf8 string.new_wtf16
             string.const string.measure_utf8 string.measure_wtf8
             string.measure_wtf16 string.concat string.eq
             string.is_usv_sequence
             string.new_lossy_utf8 string.new_wtf8
             string.as_wtf8 stringview_wtf8.advance stringview_wtf8.slice
             string.as_wtf16 stringview_wtf16.length
             stringview_wtf16.get_codeunit stringview_wtf16.slice
             ;; Assume that stateful iter stringviews are ephemeral.
             string.as_iter stringview_iter.next stringview_iter.advance
             stringview_iter.rewind stringview_iter.slice
             string.compare string.from_code_point
             string.new_utf8_array string.new_wtf16_array
             string.new_lossy_utf8_array string.new_wtf8_array

             i8x16.splat i16x8.splat i32x4.splat i64x2.splat
             f32x4.splat f64x2.splat
             ;; A number of SIMD opcodes missing here.

             i32.trunc_sat_f32_s i32.trunc_sat_f32_u
             i32.trunc_sat_f64_s i32.trunc_sat_f64_u
             i64.trunc_sat_f32_s i64.trunc_sat_f32_u
             i64.trunc_sat_f64_s i64.trunc_sat_f64_u
             table.size)
            #t)
           (else #f)))))
    (and-map effect-free-inst? expr))
  (match args
    ((($ <const> _ wat) . args)
     (match (false-if-exception (wat->wasm (list wat)))
       (($ <wasm> () ()
           (($ <func> id ($ <type-use> #f
                            ($ <func-sig> params results))
               locals body))
           () () () () #f () () () () ())
        (and (= (length params) (length args))
             (effect-free-expr? body)))
       (_ #f)))))

(define (wasm-import-effect-free? args)
  (match args
    (() #t)
    (_ #f)))

(define install-inline-wasm!
  (let ((m (current-module)))
    (lambda ()
      ;; The useless reference is to prevent a warning that (language
      ;; tree-il primitives) is unused; we just import the module so that we
      ;; can add %inline-asm as a primitive, because for reasons I don't
      ;; understand, you can't call add-interesting-primitive! from within
      ;; the compilation unit that defines the primitive.
      %inline-wasm
      (save-module-excursion
       (lambda ()
         (set-current-module m)
         (add-interesting-primitive! '%inline-wasm)
         (add-interesting-primitive! '%wasm-import)
         (add-primcall-effect-analyzer! '%inline-wasm inline-wasm-effect-free?)
         (add-primcall-effect-analyzer! '%wasm-import wasm-import-effect-free?))))))

(define (n-valued-continuation cps src nvals k)
  (define (enumerate f n)
    (let lp ((i 0))
      (if (< i n)
          (cons (f i) (lp (1+ i)))
          '())))
  (match (intmap-ref cps k)
    (($ $ktail)
     (let ((names (enumerate (lambda (n) 'result) nvals))
           (temps (enumerate (lambda (n) (fresh-var)) nvals)))
       (with-cps cps
         (letk k* ($kargs names temps
                    ($continue k src ($values temps))))
         k*)))
    (($ $kreceive ($ $arity req () rest () #f) kargs)
     (cond
      ((and (not rest) (= nvals (length req)))
       (with-cps cps
         kargs))
      ((and rest (= nvals (length req)))
       (let ((names (enumerate (lambda (n) 'result) nvals))
             (temps (enumerate (lambda (n) (fresh-var)) nvals)))
         (with-cps cps
           (letv rest)
           (letk knil ($kargs ('rest) (rest)
                         ($continue kargs src
                           ($values ,(append temps (list rest))))))
           (letk k ($kargs names temps
                     ($continue knil src ($const '()))))
           k)))
      ((and rest (zero? (length req)))
       ;; Very annoyingly, this can happen as a result of the
       ;; compilation of e.g. (letrec ((x A)) B), where X is not used in
       ;; B.  This gets compiled to (<seq> A B), and when the CPS
       ;; converter doesn't know that A is zero-valued, it just makes a
       ;; (lambda ignored B) continuation.  This happens to us when
       ;; prelude bindings that are inline-wasm forms are unused in a
       ;; user program.  So, we cons it up!
       (let ((names (enumerate (lambda (n) 'result) nvals))
             (temps (enumerate (lambda (n) (fresh-var)) nvals)))
         (define (cons-values cps temps k)
           (match temps
             (()
              (with-cps cps
                (build-term
                  ($continue k src ($const '())))))
             ((temp . temps)
              (with-cps cps
                (letv rest)
                (letk kcons ($kargs ('rest) (rest)
                              ($continue k src
                                ($primcall 'cons #f (temp rest)))))
                ($ (cons-values temps kcons))))))
         (with-cps cps
           (let$ term (cons-values temps kargs))
           (letk k ($kargs names temps ,term))
           k)))
      (else
       (error "unexpected continuation for n-valued result" nvals))))))

(define-syntax-rule (assert-match x pat message)
    (match x
      (pat #t)
      (_ (error message x))))

(define-custom-primcall-converter (%inline-wasm cps src args convert-args k)
  (define (unpack-arg cps arg type have-arg)
    (match type
      (($ <ref-type> _ _)
       (have-arg cps arg))
      ((or 'i32 'i64)
       (with-cps cps
         (letv val)
         (let$ cont (have-arg val))
         (letk kval ($kargs ('val) (val) ,cont))
         (build-term
           ($continue kval src ($primcall 'scm->u64/truncate #f (arg))))))
      ((or 'f32 'f64)
       (with-cps cps
         (letv val)
         (let$ cont (have-arg val))
         (letk kval ($kargs ('val) (val) ,cont))
         (build-term
           ($continue kval src ($primcall 'scm->f64 #f (arg))))))
      (_
       (error "invalid param type for inline wasm" type))))
  (define (unpack-args cps args types have-args)
    (match args
      (() (have-args cps '()))
      ((arg . args)
       (match types
         ((type . types)
          (unpack-arg cps arg type
                     (lambda (cps arg)
                       (unpack-args cps args types
                                   (lambda (cps args)
                                     (have-args cps (cons arg args)))))))))))
  (define (pack-result cps result type have-result)
    (match type
      (($ <ref-type> #f 'eq)
       (have-result cps result))
      ('i64
       (with-cps cps
         (letv val)
         (let$ cont (have-result val))
         (letk kval ($kargs ('val) (val) ,cont))
         (build-term
           ($continue kval src ($primcall 's64->scm #f (result))))))
      ('f64
       (with-cps cps
         (letv val)
         (let$ cont (have-result val))
         (letk kval ($kargs ('val) (val) ,cont))
         (build-term
           ($continue kval src ($primcall 'f64->scm #f (result))))))
      (_
       (error "invalid result type for inline wasm" type))))
  (define (pack-results cps results types have-results)
    (match results
      (() (have-results cps '()))
      ((result . results)
       (match types
         ((type . types)
          (pack-result
           cps result type
           (lambda (cps result)
             (pack-results cps results types
                           (lambda (cps results)
                             (have-results cps (cons result results)))))))))))
  (match args
    ((($ <const> _ code) . args)
     (assert-match code ('func . _)
                   "inline-wasm: expected a single (func ...)")
     (match (wat->wasm (list code))
       ;; We expect a single func and no other definitions (types,
       ;; tables, etc).
       (($ <wasm> () ()
           ((and func ($ <func> id ($ <type-use> #f
                                      ($ <func-sig> params results))
                         locals body)))
           () () () () #f () () () () ())
        (unless (= (length params) (length args))
          (error "inline asm with incorrect number of args" code))
        (convert-args cps args
          (lambda (cps args)
            (unpack-args
             cps args (map param-type params)
             (lambda (cps args)
               (define result-names (map (lambda (_) #f) results))
               (define result-vars (map (lambda (_) (fresh-var)) results))
               (with-cps cps
                 (let$ k* (n-valued-continuation src (length results) k))
                 (let$ pack (pack-results
                             result-vars results
                             (lambda (cps vars)
                               (with-cps cps
                                 (build-term
                                   ($continue k* src ($values vars)))))))
                 (letk k** ($kargs result-names result-vars ,pack))
                 (build-term
                   ($continue k** src
                     ($primcall 'inline-wasm func args)))))))))))))

(define-custom-primcall-converter (%wasm-import cps src args convert-args k)
  (match args
    ((($ <const> _ code) . args)
     (assert-match code ('func . _) "wasm-import: expected a single (func ...)")
     (assert-match args () "wasm-import: expected 0 args")
     (match (wat->wasm (list code))
       ;; We expect only a single import.
       (($ <wasm> () ((and import ($ <import> mod name kind id type)))
           () () () () () #f () () () () ())
        (with-cps cps
                  (let$ k* (n-valued-continuation src 0 k))
                  (build-term
                   ($continue k* src ($primcall 'wasm-import import ())))))))))
