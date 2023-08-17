;;; Pass to make all return continuations have the same type
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
;;; For WebAssembly, we CPS-convert our programs in such a way that we
;;; end up with an explicit stack; a "return" gets translated to popping
;;; a function off a stack, then tail-calling it.  To put it more
;;; generally, return continuations are stack-allocated.  Return
;;; continuations consist of data and code.  The data can be s64, u64,
;;; f64, or scm.  We use two stacks to represent the data: one for
;;; numeric values (s64, u64, f64) and one for values managed by the
;;; garbage collector (scm).
;;;
;;; What to do about code, though?  What type of stack to use there?
;;; Well in general when you return from a function you don't know how
;;; many values the calling function is expecting.  So the usual
;;; protocol is to have the return continuation take multiple values.
;;; For WebAssembly this will be our $kvarargs calling convention.
;;;
;;; However it is possible for some return continuations to be
;;; "well-known", in the sense that they know all their callers.  If
;;; they can also prove that all callers pass a compatible number of
;;; arguments (return values), then the return continuation can elide
;;; the number-of-values check.  This is the return-types optimization
;;; from (language cps return-types), which allows $call to continue to
;;; $kargs instead of $kreceive.
;;; 
;;; Bringing it back to WebAssembly, this means that the type for return
;;; continuation code can be non-uniform in the presence of return-type
;;; optimization.  We could use multiple stacks, but that gets tricky;
;;; really one starts to pine for the proper call stack which is
;;; appropriately polymorphic.  But until then, this pass undoes a bit
;;; of return-type optimization by wrapping well-known continuations in
;;; a $kclause when they are placed on a return stack.
;;;
;;; Code:

(define-module (language cps hoot unify-returns)
  #:use-module (ice-9 match)
  #:use-module (language cps)
  #:use-module (language cps intmap)
  #:use-module (language cps utils)
  #:use-module (language cps with-cps)
  #:export (unify-returns))

(define (unify-returns cps)
  (define (strip-meta cps k)
    (define (strip meta)
      (match meta
        (() '())
        (((k' . v') . meta)
         (let ((meta (strip meta)))
           (if (eq? k' k)
               meta
               (acons k' v' meta))))))
    (intmap-map (lambda (label cont)
                  (rewrite-cont cont
                    (($ $kfun src meta self ktail kentry)
                     ($kfun src (strip meta) self ktail kentry))
                    (_ ,cont)))
                cps))
  (define (maybe-wrap-return-continuation out wrapped kfun failure success)
    (match (intmap-ref wrapped kfun (lambda (_) #f))
      (#f
       (match (intmap-ref cps kfun)
         (($ $kfun src meta self ktail kentry)
          (match (intmap-ref cps kentry)
            (($ $kargs names vars term)
             (let* ((self (and self (fresh-var)))
                    (vars (map (lambda (_) (fresh-var)) vars))
                    (meta (acons 'elide-arity-check? #t meta)))
               (with-cps out
                 (letk ktail
                       ($ktail))
                 (letk kcall
                       ($kargs names vars
                         ($continue ktail src
                           ($callk kfun self vars))))
                 (letk kclause
                       ($kclause (names '() #f '() #f) kcall #f))
                 (letk kwrapped
                       ($kfun src meta self ktail kclause))
                 ($ (success (intmap-add wrapped kfun kwrapped) kwrapped)))))
            (_ (failure))))))
      (kwrapped
       (success out wrapped kwrapped))))

  (with-fresh-name-state cps
    (values
     (persistent-intmap
      (intmap-fold
       (lambda (label cont out wrapped)
         (match cont
           (($ $kargs names vars ($ $continue k src ($ $code kfun)))
            (maybe-wrap-return-continuation
             out wrapped kfun
             (lambda ()
               (values out wrapped))
             (lambda (out wrapped kwrapped)
               (with-cps out
                 (setk label
                       ($kargs names vars
                         ($continue k src ($code kwrapped))))
                 (intmap-add wrapped kfun kwrapped)))))
           (_ (values out wrapped))))
       cps
       (strip-meta cps 'elide-arity-check?)
       empty-intmap)))))
