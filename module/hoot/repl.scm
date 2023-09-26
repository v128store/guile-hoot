;;; REPL commands
;;; Copyright (C) 2023 David Thompson <dave@spritely.institute>
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
;;; Handy REPL commands for development.
;;;
;;; Code:

(define-module (hoot repl)
  #:use-module (hoot reflect)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system repl command)
  #:use-module (system repl common)
  #:use-module (system repl debug)
  #:use-module (system repl repl)
  #:use-module (wasm dump)
  #:use-module (wasm types)
  #:use-module (wasm vm))

(define (for-each/index proc lst)
  (let loop ((lst lst) (i 0))
    (match lst
      (() *unspecified*)
      ((x . rest)
       (proc i x)
       (loop rest (+ i 1))))))

(define (print-list proc title items)
  (format #t "~a:\n" title)
  (for-each/index (lambda (i item)
                    (format #t "  ~a:\t" i)
                    (proc item))
                  items))

(define (print-stack stack)
  (match (wasm-stack-items stack)
    (() (display "Empty stack.\n"))
    (items
     (print-list (lambda (x) (format #t "~s\n" x))
                 "Value stack"
                 items))))

(define (print-locals locals)
  (if (zero? (vector-length locals))
      (display "No locals.\n")
      (print-list (lambda (x) (format #t "~s\n" x))
                  "Locals"
                  (vector->list locals))))

(define (print-runtime-error e)
  (print-exception (current-output-port) #f
                   (exception-kind e)
                   (exception-args e))
  (newline)
  (print-stack (wasm-runtime-error-stack e))
  (newline)
  (print-locals (wasm-runtime-error-locals e))
  (newline)
  (print-location (wasm-module-wasm
                   (wasm-instance-module
                    (wasm-runtime-error-instance e)))
                  (wasm-runtime-error-position e)))

(define-syntax-rule (with-exception-handling body ...)
  (with-exception-handler (lambda (e) (print-runtime-error e))
    (lambda () body ...)
    #:unwind? #t
    #:unwind-for-type &wasm-runtime-error))

(define (block-type-repr type)
  (match type
    ((? func-sig?)
     (match (type-repr type)
       (('func params+results ...)
        params+results)))
    ((? ref-type?)
     `((param ,(val-type-repr type))))
    (_ `((param ,type)))))

(define (print-location wasm path)
  (define invalid-path '(-1))
  (define (path-remainder path i)
    (match path
      ((idx . rest)
       (if (and (= idx i) (not (null? rest))) rest invalid-path))))
  (define (here? path i)
    (match path
      ((idx) (= i idx))
      (_ #f)))
  (define (indent level)
    (unless (= level 0)
      (display "  ")
      (indent (- level 1))))
  (define (print-block-type type)
    (for-each (lambda (x)
                (format #t " ~s" x))
              (block-type-repr type)))
  (define (print-instr level instr path)
    (match instr
      (((and op (or 'block 'loop)) _ (or ($ <type-use> _ sig) sig) body)
       (format #t "(~a" op)
       (print-block-type sig)
       (newline)
       (print-instrs (+ level 1) body path)
       (display ")"))
      (('if _ (or ($ <type-use> _ sig) sig) consequent alternate)
       (display "(if")
       (print-block-type sig)
       (unless (null? consequent)
         (newline)
         (indent (+ level 1))
         (display "(then\n")
         (print-instrs (+ level 2) consequent
                       (path-remainder path 0))
         (display ")"))
       (unless (null? alternate)
         (newline)
         (indent (+ level 1))
         (display "(else\n")
         (print-instrs (+ level 2) alternate
                       (path-remainder path 1))
         (display ")"))
       (display ")"))
      (_
       (write instr))))
  (define (print-instrs level instrs path)
    (indent level)
    (let loop ((instrs instrs)
               (i 0))
      (match instrs
        (() #t)
        ((instr . rest)
         (if (here? path i)
             (begin
               (display "<<< ")
               (print-instr level instr (path-remainder path i))
               (display " >>>"))
             (print-instr level instr (path-remainder path i)))
         (unless (null? rest)
           (newline)
           (indent level)
           (loop rest (+ i 1)))))))
  (define (count-imports kind)
    (fold (lambda (i sum)
            (match i
              (($ <import> _ _ k)
               (if (eq? kind k) (+ sum 1) sum))))
          0 (wasm-imports wasm)))
  (match path
    (('func idx . path*)
     (match (list-ref (wasm-funcs wasm) (- idx (count-imports 'func)))
       (($ <func> id ($ <type-use> _ sig) locals body)
        (format #t "(func ~a" idx)
        (print-block-type sig)
        (newline)
        (print-instrs 1 body path*)
        (display ")"))))
    (('global idx . path*)
     (match (list-ref (wasm-globals wasm) (- idx (count-imports 'global)))
       (($ <global> id ($ <global-type> mutable? type) init)
        (let ((t (val-type-repr type)))
          (format #t "(global ~a " idx)
          (write (if mutable? `(mut ,t) t))
          (newline)
          (print-instrs 1 init path*)
          (display ")")))))
    (('data idx . path*)
     (match (list-ref (wasm-datas wasm) idx)
       (($ <data> id mode mem offset init)
        (format #t "(data ~a ~a ~a ~a\n" idx mode mem offset)
        (print-instrs 1 init path*)
        (display ")"))))
    (('elem idx j . path*)
     (match (list-ref (wasm-elems wasm) idx)
       (($ <elem> id mode table type offset inits)
        (let ((t (val-type-repr type)))
          (format #t "(elem ~a ~a ~a ~a" idx mode table t)
          (when offset
            (newline)
            (print-instrs 1 offset (if (= j 0) path* invalid-path)))
          (let loop ((inits inits) (i 1))
            (match inits
              (() #t)
              ((init . rest)
               (newline)
               (print-instrs 1 init (if (= j 1) path* invalid-path))
               (loop rest (+ i 1)))))
          (display ")"))))))
  (newline))

(define (wasm-trace path instr instance stack blocks locals)
  (define (obj-repr obj)
    (match obj
      ((? wasm-struct? struct)
       `(struct ,(object-address struct)))
      ((? wasm-array? array)
       `(array ,(object-address array)))
      ((? wasm-func? func)
       `(func ,(object-address func)))
      ((? wasm-null? null) 'null)
      (_ obj)))
  (let ((instr (match instr ; abbreviate blocks
                 (((and (or 'block 'loop) op) _ type . _)
                  `(,op ,(block-type-repr type) ...))
                 (('if _ type . _)
                  `(if ,(block-type-repr type) ...))
                 (_ instr)))
        (stack (map obj-repr (wasm-stack-items stack)))
        (locals (map obj-repr (vector->list locals))))
    (format #t "⌄ instr:  ~a\n" instr)
    (format #t "  loc:    ~a @ ~a\n" instance (reverse path))
    (format #t "  stack:  ~s\n" stack)
    (format #t "  locals: ~a\n" locals)))

(define (->wasm x)
  (match x
    ((? wasm? wasm) wasm)
    ((? wasm-module? mod) (wasm-module-wasm mod))
    ((? wasm-instance? instance)
     (wasm-module-wasm (wasm-instance-module instance)))
    ((? hoot-module? mod)
     (wasm-module-wasm
      (wasm-instance-module
       (hoot-module-instance mod))))))

(define-record-type <wasm-debug>
  (make-wasm-debug position instruction instance stack blocks locals)
  wasm-debug?
  (position wasm-debug-position)
  (instruction wasm-debug-instruction)
  (instance wasm-debug-instance)
  (stack wasm-debug-stack)
  (blocks wasm-debug-blocks)
  (locals wasm-debug-locals)
  (continue? wasm-debug-continue? set-wasm-debug-continue!))

(define current-wasm-debug (make-parameter #f))

(define-syntax-rule (when-debugging body ...)
  (if (current-wasm-debug)
      (begin body ...)
      (error "not in a WASM debugger")))

;; This code is based on error-string in (system repl
;; exception-handling) and adapted to work with Guile's new exception
;; objects.
(define (error-message exn stack)
  (let ((key (exception-kind exn))
        (args (exception-args exn)))
    (call-with-output-string
      (lambda (port)
        (let ((frame (and (< 0 (vector-length stack)) (vector-ref stack 0))))
          (print-exception port frame key args))))))

(define (enter-wasm-debugger exn)
  (let* ((tag (and (pair? (fluid-ref %stacks))
                   (cdr (fluid-ref %stacks))))
         (stack (stack->vector (make-stack #t 3 tag 0 1)))
         (msg (error-message exn stack))
         (wasm-debug (make-wasm-debug (wasm-runtime-error-position exn)
                                      (wasm-runtime-error-instruction exn)
                                      (wasm-runtime-error-instance exn)
                                      (wasm-runtime-error-stack exn)
                                      (wasm-runtime-error-blocks exn)
                                      (wasm-runtime-error-locals exn))))
    (parameterize ((current-wasm-debug wasm-debug))
      (format #t "~a\n" msg)
      (format #t "Entering WASM debug prompt. ")
      (format #t "Type `,help wasm' for info or `,q' to continue.\n")
      (start-repl #:debug (make-debug stack 0 msg))
      (wasm-debug-continue? wasm-debug))))

(define (wasm-step position instruction instance stack blocks locals)
  (let ((wasm-debug (make-wasm-debug (reverse position) instruction instance stack
                                     blocks locals)))
    (parameterize ((current-wasm-debug wasm-debug))
      (format #t "Instruction: ~a\n" instruction)
      (format #t "Location: ~a\n" (reverse position))
      (start-repl))))

(define (reset-instruction-listener)
  (current-instruction-listener
   (lambda (position instr instance stack blocks locals) #t)))

(define (continue)
  (set-wasm-debug-continue! (current-wasm-debug) #t)
  (throw 'quit))

(define-meta-command ((wasm-dump wasm) repl #:optional exp)
  "wasm-dump [WASM]
Display information about WASM, or the current WASM instance when debugging."
  (dump-wasm (->wasm
              (cond
               (exp (repl-eval repl exp))
               ((current-wasm-debug) => wasm-debug-instance)
               (else (error "no WASM object specified"))))
             #:dump-func-defs? #f))

(define-meta-command ((wasm-trace wasm) repl exp)
  "wasm-trace EXP
Evaluate EXP with verbose WASM tracing enabled."
  (with-exception-handling
   (parameterize ((current-instruction-listener wasm-trace))
     (call-with-values (lambda () (repl-eval repl exp))
       (lambda vals
         (for-each (lambda (v) (repl-print repl v)) vals))))))

(define-meta-command ((wasm-freq wasm) repl exp)
  "wasm-freq EXP
Evaluate EXP and count how many times each WASM instruction is evaluated."
  (let ((count 0)
        (histogram (make-hash-table)))
    (define (wasm-stats path instr instance stack blocks locals)
      (set! count (+ count 1))
      (match instr
        ((op . _)
         (hashq-set! histogram op (+ (hashq-ref histogram op 0) 1)))))
    (with-exception-handling
     (parameterize ((current-instruction-listener wasm-stats))
       (call-with-values (lambda () (repl-eval repl exp))
         (lambda vals
           (display "op\tcount\n")
           (display "--\t-----\n")
           (for-each (match-lambda
                       ((op . k)
                        (format #t "~a\t~a\n" op k)))
                     (sort (hash-fold alist-cons '() histogram)
                           (lambda (a b) (< (cdr a) (cdr b)))))
           (format #t "\n~a instructions total\n\n" count)
           (for-each (lambda (v) (repl-print repl v)) vals)))))))

(define-meta-command ((wasm-catch wasm) repl exp)
  "wasm-catch EXP
Catch and debug WASM runtime errors that are raised by evaluating EXP."
  (let ((thunk (repl-prepare-eval-thunk repl exp)))
    (call/ec
     (lambda (return)
       (with-exception-handler (lambda (exn)
                                 (if (wasm-runtime-error? exn)
                                     (unless (enter-wasm-debugger exn)
                                       (reset-instruction-listener)
                                       (return))
                                     (raise-exception exn)))
         (lambda ()
           (call-with-values (lambda () (%start-stack #t thunk))
             (lambda vals
               (reset-instruction-listener)
               (for-each (lambda (v) (repl-print repl v)) vals)))))))))

(define-meta-command ((wasm-stack wasm) repl)
  "wasm-stack
Print the state of the WASM stack in the current context."
  (when-debugging
   (print-stack (wasm-debug-stack (current-wasm-debug)))))

(define-meta-command ((wasm-locals wasm) repl)
  "wasm-locals
Print the state of the WASM locals in the current context."
  (when-debugging
   (print-locals (wasm-debug-locals (current-wasm-debug)))))

(define-meta-command ((wasm-pos wasm) repl)
  "wasm-pos
Highlight the instruction where WASM execution has paused."
  (when-debugging
   (let ((debug (current-wasm-debug)))
     (print-location (->wasm (wasm-debug-instance debug)) (wasm-debug-position debug)))))

(define-meta-command ((wasm-eval wasm) repl instr)
  "wasm-eval INSTR
Evaluate the WASM instruction INSTR in the current debug context."
  (when-debugging
   (let ((execute (@@ (wasm vm) execute)))
     (match (current-wasm-debug)
       (($ <wasm-debug> position _ instance stack blocks locals)
        (execute (repl-eval repl instr) position instance stack blocks locals))))))

(define-meta-command ((wasm-continue wasm) repl)
  "wasm-continue
Set WASM execution to continue without interruption until the next error."
  (when-debugging
   (reset-instruction-listener)
   (when (current-wasm-debug)
     (continue))))

(define-meta-command ((wasm-step wasm) repl)
  "wasm-step
Set WASM execution to pause before each instruction."
  (when-debugging
   (current-instruction-listener wasm-step)
   (when (current-wasm-debug)
     (continue))))
