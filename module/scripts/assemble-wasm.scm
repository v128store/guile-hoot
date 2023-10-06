;;; Assemble --- Command-line Wasm assembler  -*- coding: iso-8859-1 -*-

;;; Copyright (C) 2023 Igalia, S.L.
;;; Copyright 2005,2008-2011,2013-2015,2017-2020 Free Software Foundation, Inc.
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
;;; Usage: assemble-wasm [ARGS]
;;;
;;; A command-line interface to the Guile Wasm assembler, based on
;;; Hoot's compile-wasm script.
;;;
;;; Code:

(define-module (scripts assemble-wasm)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-37)
  #:use-module ((wasm assemble) #:select ((assemble-wasm . wasm:assemble-wasm)))
  #:use-module (wasm lower)
  #:use-module (wasm wat)
  #:export (assemble-wasm))

(define %summary "Assemble a Wasm module.")


(define (fail message . args)
  (format (current-error-port) "error: ~?~%" message args)
  (exit 1))

(define %options
  ;; Specifications of the command-line options.
  (list (option '(#\h "help") #f #f
                (lambda (opt name arg result)
		  (alist-cons 'help? #t result)))
        (option '("version") #f #f
                (lambda (opt name arg result)
                  (show-version)
                  (exit 0)))

	(option '(#\o "output") #t #f
		(lambda (opt name arg result)
		  (if (assoc-ref result 'output-file)
		      (fail "`-o' option cannot be specified more than once")
		      (alist-cons 'output-file arg result))))))

(define (parse-args args)
  "Parse argument list @var{args} and return an alist with all the relevant
options."
  (args-fold args %options
             (lambda (opt name arg result)
               (format (current-error-port) "~A: unrecognized option~%" name)
	       (exit 1))
             (lambda (file result)
	       (let ((input-files (assoc-ref result 'input-files)))
		 (alist-cons 'input-files (cons file input-files)
			     result)))

	     ;; default option values
             `((input-files))))

(define (show-version)
  (format #t "assemble-wasm ~A~%" (version))
  (format #t "Copyright (C) 2023  Spritely Institute, Igalia.
Part of guile-hoot:
  https://gitlab.com/spritely/guile-hoot
Licensed under the Apache License, Version 2.0:
  http://www.apache.org/licenses/LICENSE-2.0
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.~%"))

(define (assemble-wasm . args)
  (let* ((options         (parse-args args))
         (help?           (assoc-ref options 'help?))
         (input-files     (assoc-ref options 'input-files))
	 (output-file     (assoc-ref options 'output-file)))
    (when (or help? (null? input-files))
      (format #t "Usage: assemble-wasm [OPTION] FILE
Compile the WAT source file FILE into a WebAssembly module file.

  -h, --help           print this help message

  -o, --output=OFILE   write output to OFILE

Report bugs to <~A>.~%"
              %guile-bug-report-address)
      (exit 0))

    (unless output-file
      (fail "missing output file (pass `-o FILE')"))

    ;; Install a SIGINT handler.  As a side effect, this gives unwind
    ;; handlers an opportunity to run upon SIGINT; this includes that of
    ;; 'call-with-output-file/atomic', called by 'compile-file', which
    ;; removes the temporary output file.
    (sigaction SIGINT
               (lambda args
                 (fail "interrupted by the user")))

    (match input-files
      (() (fail "missing input file"))
      ((input-file)
       (let ((expr (with-input-from-file input-file read)))
         (call-with-output-file output-file
           (cut put-bytevector <>
                (wasm:assemble-wasm (lower-wasm (wat->wasm expr))))))
       (format #t "wrote `~A'\n" output-file))
      (_ (fail "multiple input files not supported")))))

(define main assemble-wasm)
