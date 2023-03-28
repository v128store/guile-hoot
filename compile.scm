(use-modules (ice-9 format)
             (ice-9 match)
             (system base compile)
             (system base language)
             (language cps tailify)
             (language cps verify)
             (language cps renumber)
             (language cps dce)
             (language cps simplify)
             (language cps dump))

(define (compile-to-wasm cps)
  ;; for each function,
  ;;   split into blocks.
  ;;   blocks already sorted topologically because of renumbering.
  ;;   create map of var -> defs.
  ;;   create set of multiply-used defs.
  ;;   define set of stack-allocated defs, initially empty.
  ;;   then in post-order,
  ;;     for each block,
  ;;       for each instruction in reverse order,
  ;;         collect operands:
  ;;           either on stack if used-once, def in same block, def commutes
  ;;           or local-ref
  ;;         emit wasm instrs for cps op, with operands
  ;; interning constants into constant table
  ;; finalizing constant table
  ;; setting init function.
  #f)

(define* (compile-to-wasm input-file output-file #:key
                          (from (current-language))
                          (env (default-environment from))
                          (optimization-level (default-optimization-level))
                          (warning-level (default-warning-level))
                          (opts '())
                          (canonicalization 'relative))
  (define (compile-to-cps in)
    ;; FIXME: Right now the tree-il->cps phase will expand
    ;; primitives to Guile VM primitives, e.g. including
    ;; `heap-object?` and so on.  We need to instead expand into
    ;; more wasm-appropriate primitives, at some point anyway.
    (define cps
      (read-and-compile in #:env env #:from from #:to 'cps
                        #:optimization-level optimization-level
                        #:warning-level warning-level))
    (define lower-cps
      (let ((make-lower (language-lowerer (lookup-language 'cps))))
        (make-lower optimization-level opts)))
    (define lowered-cps (lower-cps cps env))
    (define tailified (tailify lowered-cps))
    (verify tailified)
    (renumber (simplify (eliminate-dead-code tailified))))
  (call-with-input-file input-file
    (lambda (in)
      (set-port-encoding! in (or (file-encoding in) "UTF-8"))
      (define cps (compile-to-cps in))
      (dump cps)
      #;
      (let ((wasm (lower-to-wasm cps)))
        (call-with-output-file output-file
          (lambda (out)
            (write-wasm wasm out)))))))

(define (main args)
  (match args
    ((_ in out)
     (compile-to-wasm in out))
    ((arg0 . _)
     (format (current-error-port) "usage: ~a INPUT-FILE OUTPUT-FILE\n" arg0)
     (exit 1))))

(when (batch-mode?)
  (main (program-arguments))
  (exit 0))
