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
;;; This file exists only to be a place to define primitives for use by
;;; Hoot user code.  It also exports implementation-detail primitives
;;; for use by the Hoot standard library; eventually this will change to
;;; avoid exposing these nonstandard primitives to users.
;;;
;;; Code:

(define-module (hoot primitives)
  #:use-module (scheme base)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 atomic)
  #:re-export
  ( ;; R7RS syntax
   _
   ... => else
   lambda
   define define-values let let* letrec letrec* let-values let*-values
   or and
   begin 
   if cond case when unless
   do
   set!
   quote quasiquote unquote unquote-splicing
   include include-ci
   define-syntax let-syntax letrec-syntax
   syntax-rules syntax-error
   cond-expand
   parameterize
   ;; FIXME: These two need Hoot support.
   ;; guard
   ;; define-record-type

   ;; Most primitives can only appear in primcalls, so we expose them as
   ;; %foo instead of foo, relying on the prelude to wrap them in
   ;; lambdas to ensure they are always called with the right number of
   ;; arguments, even when used as a value.  The three exceptions are
   ;; `apply`, `abort-to-prompt`, and `values`.

   ;; Guile syntax extensions
   include-from-path
   define-syntax-rule
   syntax-case syntax quasisyntax unsyntax unsyntax-splicing
   with-syntax identifier-syntax identifier?
   lambda* define*

   ;; R7RS control
   (dynamic-wind . %dynamic-wind)

   ;; R7RS values
   values
   (call-with-values . %call-with-values)
   apply

   ;; R7RS pairs
   (pair? . %pair?)
   (cons . %cons)
   (car . %car)
   (cdr . %cdr)
   (set-car! . %set-car!)
   (set-cdr! . %set-cdr!)

   ;; R7RS lists
   (null? . %null?)

   ;; R7RS bytevectors
   (bytevector-length . %bytevector-length)
   (bytevector-u8-ref . %bytevector-u8-ref)
   (bytevector-u8-set! . %bytevector-u8-set!)
   (bytevector? . %bytevector?)

   ;; R7RS numerics
   (* . %*)
   (+ . %+)
   (- . %-)
   (/ . %/)
   (< . %<)
   (<= . %<=)
   (= . %=)
   (> . %>)
   (>= . %>=)
   (abs . %abs)
   (floor . %floor)
   (ceiling . %ceiling)
   (number? . %number?)
   (complex? . %complex?)
   (real? . %real?)
   (rational? . %rational?)
   (integer? . %integer?)
   (exact-integer? . %exact-integer?)
   (exact? . %exact?)
   (inexact? . %inexact?)
   ;; FIXME: we should actually be using the R7RS variants which are
   ;; slightly different than Guile's.
   (exact->inexact . %inexact)
   (quotient . %quotient)
   (remainder . %remainder)
   (modulo . %modulo)
   (sin . %sin)
   (cos . %cos)
   (tan . %tan)
   (asin . %asin)
   (acos . %acos)
   (atan . %atan)
   (sqrt . %sqrt)

   ;; R7RS chars
   (char->integer . %char->integer)
   (integer->char . %integer->char)
   (char? . %char?)

   ;; R7RS ports
   (eof-object? . %eof-object?)

   ;; Parameters

   ;; R7RS equality
   (eq? . %eq?)
   (eqv? . %eqv?)

   ;; R7RS strings
   (string? . %string?)
   (string-length . %string-length)
   (string-ref . %string-ref)

   ;; Symbols
   (symbol? . %symbol?)
   (symbol->string . %symbol->string)
   (string->symbol . %string->symbol)

   ;; R7RS vectors
   (vector? . %vector?)
   (make-vector . %make-vector)
   (vector . %vector)
   (vector-length . %vector-length)
   (vector-ref . %vector-ref)
   (vector-set! . %vector-set!)

   ;; Error handling
   (error . %error)

   (procedure? . %procedure?)

   ;; guile extensions
   (call-with-prompt . %call-with-prompt)
   abort-to-prompt
   (ash . %ash)
   (logand . %logand)
   (logior . %logior)
   (logxor . %logxor)
   (lognot . %lognot)
   (logtest . %logtest)
   (logbit? . %logbit?)
   (keyword? . %keyword?)
   (bitvector? . %bitvector?)
   (cons* . %cons*)
   (fluid-ref . %fluid-ref)
   (fluid-set! . %fluid-set!)
   (with-fluid* . %with-fluid*)
   (with-dynamic-state . %with-dynamic-state)
   (make-atomic-box . %make-atomic-box)
   (atomic-box-ref . %atomic-box-ref)
   (atomic-box-set! . %atomic-box-set!)
   (atomic-box-swap! . %atomic-box-swap!)
   (atomic-box-compare-and-swap! . %atomic-box-compare-and-swap!)
   ; make-variable variable-ref variable-set!
   (bytevector-s8-ref . %bytevector-s8-ref)
   (bytevector-s8-set! . %bytevector-s8-set!)
   (bytevector-u16-native-ref . %bytevector-u16-native-ref)
   (bytevector-u16-native-set! . %bytevector-u16-native-set!)
   (bytevector-s16-native-ref . %bytevector-s16-native-ref)
   (bytevector-s16-native-set! . %bytevector-s16-native-set!)
   (bytevector-u32-native-ref . %bytevector-u32-native-ref)
   (bytevector-u32-native-set! . %bytevector-u32-native-set!)
   (bytevector-s32-native-ref . %bytevector-s32-native-ref)
   (bytevector-s32-native-set! . %bytevector-s32-native-set!)
   (bytevector-u64-native-ref . %bytevector-u64-native-ref)
   (bytevector-u64-native-set! . %bytevector-u64-native-set!)
   (bytevector-s64-native-ref . %bytevector-s64-native-ref)
   (bytevector-s64-native-set! . %bytevector-s64-native-set!)
   (bytevector-ieee-single-native-ref . %bytevector-ieee-single-native-ref)
   (bytevector-ieee-single-native-set! . %bytevector-ieee-single-native-set!)
   (bytevector-ieee-double-native-ref . %bytevector-ieee-double-native-ref)
   (bytevector-ieee-double-native-set! . %bytevector-ieee-double-native-set!)
   the-eof-object
   )
  ;; Mark as non-declarative, as we should not have inlinable exports.
  #:declarative? #f)