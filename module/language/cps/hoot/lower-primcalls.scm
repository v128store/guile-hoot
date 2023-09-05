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

(define (lower-primcalls cps)
  (with-fresh-name-state cps
    (intmap-fold
     (lambda (label cont out)
       (define (emit-raise-exception out src exn)
         (with-cps out
           (letv prim)
           (letk kstop ($kargs () () ($throw src 'unreachable #f ())))
           (letk kraise ($kargs ('prim) (prim)
                          ($continue kstop src
                            ($call prim (exn)))))
           (build-term
             ($continue kraise src ($prim 'raise-exception)))))

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
             ($ $throw src 'throw #f (key args)))
          (with-cps out
            (letv exn)
            (let$ term (emit-raise-exception src exn))
            (letk kraise ($kargs ('exn) (exn) ,term))
            (setk label ($kargs names vars
                          ($continue kraise src
                            ($primcall 'make-throw-exn #f (key args)))))))

         (($ $kargs names vars
             ($ $throw src (or 'throw/value 'throw/value+data) param (val)))
          (with-cps out
            (letv exn)
            (let$ term (emit-raise-exception src exn))
            (letk kraise ($kargs ('exn) (exn) ,term))
            (setk label ($kargs names vars
                          ($continue kraise src
                            ($primcall 'make-throw/value-exn param (val)))))))
         (_ out)))
     cps
     cps)))
