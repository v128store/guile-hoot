;;; Compile --- Command-line Guile Scheme compiler  -*- coding: iso-8859-1 -*-

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
;;; Usage: compile-wasm [ARGS]
;;;
;;; A command-line interface to the Guile-to-WebAssembly compiler.
;;;
;;; Code:

(define-module (scripts compile-wasm)
  #:use-module ((system base compile) #:select (default-warning-level
                                                default-optimization-level))
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (system base message)
  #:use-module (system base optimize)
  #:use-module (hoot compile)
  #:use-module (wasm dump)
  #:use-module (wasm parse)
  #:export (compile-wasm))

(define %summary "Compile a file to WebAssembly.")


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

	(option '(#\L "load-path") #t #f
		(lambda (opt name arg result)
		  (let ((load-path (assoc-ref result 'load-path)))
		    (alist-cons 'load-path (cons arg load-path)
				result))))
	(option '(#\o "output") #t #f
		(lambda (opt name arg result)
		  (if (assoc-ref result 'output-file)
		      (fail "`-o' option cannot be specified more than once")
		      (alist-cons 'output-file arg result))))
        (option '("r6rs") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'install-r6rs? #t result)))
        (option '("dump-wasm") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'dump-wasm? #t result)))
        (option '("dump-cps") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'dump-cps? #t result)))
        (option '("r7rs") #f #f
		(lambda (opt name arg result)
		  (alist-cons 'install-r7rs? #t result)))
        (option '(#\x) #t #f
                (lambda (opt name arg result)
                  (set! %load-extensions (cons arg %load-extensions))
                  result))

        (option '(#\W "warn") #t #f
                (lambda (opt name arg result)
                  (match arg
                    ("help"
                     (show-warning-help)
                     (exit 0))
                    ((? string->number)
                     (let ((n (string->number arg)))
                       (unless (and (exact-integer? n) (<= 0 n))
                         (fail "Bad warning level `~a'" n))
                       (alist-cons 'warning-level n
                                   (alist-delete 'warning-level result))))
                    (_
                     (let ((warnings (assoc-ref result 'warnings)))
                       (alist-cons 'warnings
                                   (cons (string->symbol arg) warnings)
                                   (alist-delete 'warnings result)))))))

	(option '(#\O "optimize") #t #f
		(lambda (opt name arg result)
                  (define (return val)
                    (alist-cons 'optimizations val result))
                  (define (return-option name val)
                    (let ((kw (symbol->keyword
                               (string->symbol (string-append name "?")))))
                      (unless (assq kw (available-optimizations))
                        (fail "Unknown optimization pass `~a'" name))
                      (return (list kw val))))
                  (cond
                   ((string=? arg "help")
                    (show-optimization-help)
                    (exit 0))
                   ((string->number arg)
                    => (lambda (level)
                         (unless (and (exact-integer? level) (<= 0 level 9))
                           (fail "Bad optimization level `~a'" level))
                         (alist-cons 'optimization-level level
                                     (alist-delete 'optimization-level result))))
                   ((string-prefix? "no-" arg)
                    (return-option (substring arg 3) #f))
                   (else
                    (return-option arg #t)))))
	(option '(#\f "from") #t #f
		(lambda (opt name arg result)
                  (if (assoc-ref result 'from)
                      (fail "`--from' option cannot be specified more than once")
                      (alist-cons 'from (string->symbol arg) result))))))

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
             `((input-files)
	       (load-path)
               (warning-level . ,(default-warning-level))
               (optimization-level . ,(default-optimization-level))
               (warnings unsupported-warning))))

(define (show-version)
  (format #t "compile-wasm ~A~%" (version))
  (format #t "Copyright (C) 2023  Spritely Institute, Igalia.
Part of guile-hoot:
  https://gitlab.com/spritely/guile-hoot
Licensed under the Apache License, Version 2.0:
  http://www.apache.org/licenses/LICENSE-2.0
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.~%"))

(define (show-warning-help)
  (format #t "The available warning types are:~%~%")
  (for-each (lambda (wt)
              (format #t "  ~22A ~A~%"
                      (format #f "`~A'" (warning-type-name wt))
                      (warning-type-description wt)))
            %warning-types)
  (format #t "~%")
  (format #t "You may also specify warning levels as `-W0`, `-W1',~%")
  (format #t "`-W2', or `-W3'.  The default is `-W1'.~%"))

(define (show-optimization-help)
  (format #t "The available optimizations are:~%~%")
  (let lp ((options (available-optimizations)))
    (match options
      (() #t)
      (((kw level) . options)
       (let ((name (string-trim-right (symbol->string (keyword->symbol kw))
                                      #\?)))
         (format #t "  -O~a~%" name)
         (lp options)))))
  (format #t "~%")
  (format #t "To disable an optimization, prepend it with `no-', for example~%")
  (format #t "`-Ono-cse.'~%~%")
  (format #t "You may also specify optimization levels as `-O0', `-O1',~%")
  (format #t "`-O2', or `-O3'.  Currently `-O0' turns off all optimizations,~%")
  (format #t "`-O1' turns on partial evaluation, and `-O2' and `-O3' turn on~%")
  (format #t "everything.  The default is equivalent to `-O2'.")
  (format #t "~%"))

(define (compile-wasm . args)
  (let* ((options         (parse-args args))
         (help?           (assoc-ref options 'help?))
         (warning-level   (assoc-ref options 'warning-level))
         (optimization-level (assoc-ref options 'optimization-level))
         (dump-wasm?      (assoc-ref options 'dump-wasm?))
         (dump-cps?       (assoc-ref options 'dump-cps?))
         (compile-opts    `(#:warnings
                            ,(assoc-ref options 'warnings)
                            ,@(append-map
                               (lambda (opt)
                                 (match opt
                                   (('optimizations . opts) opts)
                                   (_ '())))
                               options)))
         (from            (or (assoc-ref options 'from) 'scheme))
         (input-files     (assoc-ref options 'input-files))
	 (output-file     (assoc-ref options 'output-file))
	 (load-path       (assoc-ref options 'load-path)))
    (when (or help? (null? input-files))
      (format #t "Usage: compile-wasm [OPTION] FILE
Compile the Guile source file FILE into a WebAssembly module file.

  -h, --help           print this help message

  -L, --load-path=DIR  add DIR to the front of the module load path
  -o, --output=OFILE   write output to OFILE
  -x EXTENSION         add EXTENSION to the set of source file extensions

  -W, --warn=WARNING   emit warnings of type WARNING; use `--warn=help'
                       for a list of available warnings
  -O, --optimize=OPT   specify optimization passes to run; use `-Ohelp'
                       for a list of available optimizations

  --r6rs, --r7rs       compile in an environment whose default bindings,
                       reader options, and load paths are adapted for
                       specific Scheme standards; see \"R6RS Support\"
                       and \"R7RS Support\" in the manual, for full details

  --dump-cps           print a debugging representation of the low-level
                       CPS code, before generating WebAssembly
  --dump-wasm          print a debugging representation of the generated
                       WebAssembly code

  -f, --from=LANG      specify a source language other than `scheme'

Report bugs to <~A>.~%"
              %guile-bug-report-address)
      (exit 0))

    (when (assoc-ref options 'install-r6rs?)
      (install-r6rs!))
    (when (assoc-ref options 'install-r7rs?)
      (install-r7rs!))

    (set! %load-path (append load-path %load-path))

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
       (with-fluids ((*current-warning-prefix* ""))
         (compile-file input-file
                       #:output-file output-file
                       #:from from
                       #:warning-level warning-level
                       #:optimization-level optimization-level
                       #:dump-cps? dump-cps?
                       #:dump-wasm? dump-wasm?
                       #:opts compile-opts))
       (format #t "wrote `~A'\n" output-file))
      (_ (fail "multiple input files not supported")))))

(define main compile-wasm)
