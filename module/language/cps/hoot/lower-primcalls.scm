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
  #:export (lower-primcalls))

(define (lower-primcalls cps)
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
       (_ out)))
   cps
   cps))
