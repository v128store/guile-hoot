;;; Pass to lower-primcalls CPS for hoot
;;; Copyright (C) 2023 Free Software Foundation, Inc.
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
;;; When targetting WebAssembly, we don't have untagged struct fields,
;;; so we can fold some vtable predicates.
;;;
;;; Code:

(define-module (language cps hoot lower-primcalls)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (lower-primcalls))

(define (hoot-fixnum? x) (and (exact-integer? x)
                              (<= (ash -1 29) x (1- (ash 1 29)))))
(define (not-hoot-fixnum? x) (not (hoot-fixnum? x)))

(define (lower-primcalls cps)
  (with-fresh-name-state cps
    (intmap-fold
     (lambda (label cont out)
       (match cont
         (($ $kargs names vars
             ($ $branch kf kt src 'vtable-has-unboxed-fields? nfields (vtable)))
          (intmap-replace out label
                          (build-cont
                            ($kargs names vars
                              ($continue kf src ($values ()))))))

         (($ $kargs names vars
             ($ $branch kf kt src 'vtable-field-boxed? idx (vtable)))
          (intmap-replace out label
                          (build-cont
                            ($kargs names vars
                              ($continue kt src ($values ()))))))

         (($ $kargs names vars
             ($ $branch kf kt src (or 'number? 'complex?) #f (x)))
          (with-cps out
            (letk kheap ($kargs () ()
                          ($branch kf kt src 'heap-number? #f (x))))
            (setk label ($kargs names vars
                          ($branch kheap kt src 'fixnum? #f (x))))))

         (($ $kargs names vars
             ($ $branch kf kt src 'real? #f (x)))
          (with-cps out
            (letk kfix? ($kargs () ()
                          ($branch kf kt src 'fixnum? #f (x))))
            (letk kcomplex? ($kargs () ()
                              ($branch kt kf src 'compnum? #f (x))))
            (setk label
                  ($kargs names vars
                    ($branch kfix? kcomplex? src 'heap-number? #f (x))))))

         (($ $kargs names vars
             ($ $branch kf kt src 'rational? #f (x)))
          (with-cps out
            (letv real imag)
            (letk kreal-finite? ($kargs ('real) (real)
                                  ($branch kf kt src 'f64-finite? #f (real))))
            (letk kflo ($kargs () ()
                         ($continue kreal-finite? src
                           ($primcall 'flonum->f64 #f (x)))))
            (letk kcomp-real ($kargs () ()
                               ($continue kreal-finite? src
                                 ($primcall 'compnum-real #f (x)))))
            (letk kimag-finite? ($kargs ('imag) (imag)
                                  ($branch kf kcomp-real src 'f64-finite? #f (imag))))
            (letk kcomp ($kargs () ()
                          ($continue kimag-finite? src
                            ($primcall 'compnum-imag #f (x)))))
            (letk knum? ($kargs () ()
                           ($branch kf kt src 'heap-number? #f (x))))
            (letk kcomp? ($kargs () ()
                           ($branch knum? kcomp src 'compnum? #f (x))))
            (letk kflo? ($kargs () ()
                          ($branch kcomp? kflo src 'flonum? #f (x))))
            (setk label ($kargs names vars
                          ($branch kflo? kt src 'fixnum? #f (x))))))

         (($ $kargs names vars
             ($ $branch kf kt src 'integer? #f (x)))
          (with-cps out
            (letv real imag)
            (letk kreal-int? ($kargs ('real) (real)
                                  ($branch kf kt src 'f64-int? #f (real))))
            (letk kflo ($kargs () ()
                         ($continue kreal-int? src
                           ($primcall 'flonum->f64 #f (x)))))
            (letk kcomp-real ($kargs () ()
                               ($continue kreal-int? src
                                 ($primcall 'compnum-real #f (x)))))
            (letk kimag-int? ($kargs ('imag) (imag)
                                  ($branch kf kcomp-real src 'f64-int? #f (imag))))
            (letk kcomp ($kargs () ()
                          ($continue kimag-int? src
                            ($primcall 'compnum-imag #f (x)))))
            (letk kcomp? ($kargs () ()
                           ($branch kf kcomp src 'compnum? #f (x))))
            (letk kflo? ($kargs () ()
                          ($branch kcomp? kflo src 'flonum? #f (x))))
            (letk kbig? ($kargs () ()
                          ($branch kflo? kt src 'bignum? #f (x))))
            (setk label ($kargs names vars
                          ($branch kbig? kt src 'fixnum? #f (x))))))

         (($ $kargs names vars
             ($ $branch kf kt src 'exact-integer? #f (x)))
          (with-cps out
            (letk kbig? ($kargs () ()
                          ($branch kf kt src 'bignum? #f (x))))
            (setk label ($kargs names vars
                          ($branch kbig? kt src 'fixnum? #f (x))))))

         (($ $kargs names vars
             ($ $branch kf kt src 'exact? #f (x)))
          (with-cps out
            (letk kfrac? ($kargs () ()
                           ($branch kf kt src 'fracnum? #f (x))))
            (letk kbig? ($kargs () ()
                          ($branch kfrac? kt src 'bignum? #f (x))))
            (setk label ($kargs names vars
                          ($branch kbig? kt src 'fixnum? #f (x))))))

         (($ $kargs names vars
             ($ $branch kf kt src 'inexact? #f (x)))
          (with-cps out
            (letk kcomp? ($kargs () ()
                          ($branch kf kt src 'compnum? #f (x))))
            (setk label ($kargs names vars
                          ($branch kcomp? kt src 'flonum? #f (x))))))

         (($ $kargs names vars
             ($ $continue k src ($ $primcall 'call-thunk/no-inline #f (thunk))))
          (intmap-replace out label
                          (build-cont
                            ($kargs names vars
                              ($continue k src ($call thunk ()))))))

         (($ $kargs names vars
             ($ $continue k src ($ $primcall 'load-const/unlikely val ())))
          (with-cps out
            (setk label ($kargs names vars ($continue k src ($const val))))))

         (($ $kargs names vars
             ($ $continue k src ($ $primcall 'tag-fixnum/unlikely #f (val))))
          (with-cps out
            (setk label ($kargs names vars
                          ($continue k src
                            ($primcall 'tag-fixnum #f (val)))))))

         (($ $kargs names vars
             ($ $continue k src ($ $primcall 'u64->scm/unlikely #f (val))))
          (with-cps out
            (setk label ($kargs names vars
                          ($continue k src ($primcall 'u64->scm #f (val)))))))
         (($ $kargs names vars
             ($ $continue k src ($ $primcall 's64->scm/unlikely #f (val))))
          (with-cps out
            (setk label ($kargs names vars
                          ($continue k src ($primcall 's64->scm #f (val)))))))

         (($ $kargs names vars
             ($ $continue k src
                ($ $primcall (or 'assume-u64 'assume-s64) (lo . hi) (val))))
          (with-cps out
            (setk label ($kargs names vars
                          ($continue k src ($values (val)))))))

         (($ $kargs names vars
             ($ $continue k src
                ($ $primcall 'add/immediate (? not-hoot-fixnum? y) (x))))
          (with-cps out
            (letv y*)
            (letk k* ($kargs ('y) (y*)
                       ($continue k src ($primcall 'add #f (x y*)))))
            (setk label ($kargs names vars
                          ($continue k* src ($const y))))))

         (($ $kargs names vars
             ($ $continue k src
                ($ $primcall 'sub/immediate (? not-hoot-fixnum? y) (x))))
          (with-cps out
            (letv y*)
            (letk k* ($kargs ('y) (y*)
                       ($continue k src ($primcall 'sub #f (x y*)))))
            (setk label ($kargs names vars
                          ($continue k* src ($const y))))))

         (($ $kargs names vars
             ($ $continue k src
                ($ $primcall 'mul/immediate (? not-hoot-fixnum? y) (x))))
          (with-cps out
            (letv y*)
            (letk k* ($kargs ('y) (y*)
                       ($continue k src ($primcall 'mul #f (x y*)))))
            (setk label ($kargs names vars
                          ($continue k* src ($const y))))))

         (($ $kargs names vars
             ($ $continue k src
                ($ $primcall 'logand/immediate (? not-hoot-fixnum? y) (x))))
          (with-cps out
            (letv y*)
            (letk k* ($kargs ('y) (y*)
                       ($continue k src ($primcall 'logand #f (x y*)))))
            (setk label ($kargs names vars
                          ($continue k* src ($const y))))))

         (($ $kargs names vars ($ $throw src op param args))
          (match op
            ((or 'raise-type-error
                 'raise-range-error
                 'raise-arity-error
                 'raise-exception) out)
            (_ (error "unexpected throw; fix to use raise-exception" op))))

         (_ out)))
     cps
     cps)))
