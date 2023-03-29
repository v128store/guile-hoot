(use-modules (ice-9 format)
             (ice-9 match)
             (ice-9 pretty-print)
             (system base compile)
             (system base language)
             (language cps)
             (language cps intset)
             (language cps intmap)
             (language cps tailify)
             (language cps verify)
             (language cps renumber)
             (language cps dce)
             (language cps simplify)
             (language cps dump)
             (language cps utils))

(define (invert-tree parents)
  (intmap-fold
   (lambda (child parent tree)
     (let ((tree (intmap-add tree child empty-intset intset-union)))
       (match parent
         (-1 tree)
         (_ (intmap-add tree parent (intset child) intset-union)))))
   parents empty-intmap))

(define (intset-filter pred set)
  (persistent-intset
   (intset-fold (lambda (i out)
                  (if (pred i) (intset-add! out i) out))
                set empty-intset)))

(define (intset-pop set)
  (match (intset-next set)
    (#f (values set #f))
    (i (values (intset-remove set i) i))))

(define (lower-to-wasm cps)
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
  (define strings '())
  (define heap-constants '())
  (define funcs
    (intmap-map
     (lambda (kfun body)
       (let ((cps (intmap-select cps body)))
         (define preds (compute-predecessors cps kfun))
         (define idoms (compute-idoms cps kfun))
         (define dom-children (invert-tree idoms))
         (define (merge-cont? label)
           (let lp ((preds (intmap-ref preds label))
                    (has-forward-in-edge? #f))
             (match preds
               (() #f)
               ((pred . preds)
                (if (< pred label)
                    (or has-forward-in-edge?
                        (lp preds #t))
                    (lp preds has-forward-in-edge?))))))
         (define (loop-cont? label)
           (or-map (lambda (pred) (<= label pred))
                   (intmap-ref preds label)))
         (define (loop-label label)
           (string->symbol (format #f "$l~a" label)))
         (define (wrap-loop expr label)
           (if (loop-cont? label)
               `(loop ,(loop-label label) ,expr)
               expr))
         (define (var-label var) (string->symbol (format #f "$v~a" var)))
         (define (local.get var) `(local.get ,(var-label var)))
         (define (local.set var) `(local.set ,(var-label var)))
         (define (arg-ref idx)
           (if (< idx 3)
               `(local.get ,(string->symbol (format #f "$arg~a" idx)))
               `(array.get $argv (global.get $argv) (i32.const ,(- idx 4)))))
         (define (func-label k) (string->symbol (format #f "$f~a" k)))
         (define (compile-tail exp)
           (match exp
             (($ $call proc args)
              `(return_call_ref ,@(map local.get args)
                                (struct.get $proc 1
                                            (ref.cast $proc
                                                      ,(local.get proc)))))
             (($ $calli args callee)
              ;; This is a return.
              `(return_call_ref ,@(map local.get args) ,(local.get callee)))
             (($ $callk k proc args)
              `(return_call ,(func-label k)
                            ,@(map local.get
                                   (if proc (cons proc args) args))))))
         (define (compile-values exp)
           (define (fixnum? val)
             (and (exact-integer? val)
                  (<= (ash -1 -29) val (1- (ash 1 29)))))
           (match exp
             (($ $const val)
              (match val
                ((? fixnum?) `(i31.new (i32.const ,(ash val 1))))
                (_ (error "unimplemented constant" val))))
             (($ $primcall 'restore1 'ptr ())
              `(call $pop-return!))
             (_
              (error "unimplemented!" exp))))
         (define (compile-receive exp req rest kargs)
           (match exp
             (_
              (error "unimplemented!!" exp req rest kargs))))
         (define (compile-test op param args)
           (match op
             (_ (error "unimplemented!!!" op param args))))
         
         ;; See "Beyond Relooper: Recursive Translation of Unstructured
         ;; Control Flow to Structured Control Flow", Norman Ramsey, ICFP
         ;; 2022.
         (define (make-ctx next-label stack) (cons next-label stack))
         (define (push-loop label ctx)
           (match ctx
             ((next-label . stack)
              (make-ctx label
                        (acons 'loop-headed-by label stack)))))
         (define (push-block label ctx)
           (match ctx
             ((next-label . stack)
              (make-ctx label
                        (acons 'block-followed-by label stack)))))
         (define (push-if label ctx)
           (match ctx
             ((next-label . stack)
              (make-ctx next-label (cons 'if-then-else stack)))))
         (define (lookup-label k ctx)
           (match ctx
             ((next-label . stack)
              (let lp ((stack stack) (depth 0))
                (match stack
                  (('if-then-else . stack) (lp stack (1+ depth)))
                  ((((or 'loop-headed-by 'block-followed-by) label) . stack)
                   (if (eqv? label k)
                       depth
                       (lp stack (1+ depth))))
                  (_ (error "block label not found" k)))))))

         (define (do-tree label ctx)
           (define (code-for-label ctx)
             ;; here if label is a switch we node-within all children
             ;; instead of only merge nodes.
             (define children
               (intset-filter merge-cont? (intmap-ref dom-children label)))
             (node-within label children ctx))
           (if (loop-cont? label)
               `(loop ,(code-for-label (push-loop label ctx)))
               (code-for-label ctx)))
         (define (do-branch pred succ ctx)
           (cond
            ((or (<= succ pred)
                 (merge-cont? succ))
             ;; Backward branch or branch to merge: jump.
             (match ctx
               ((next-label . stack)
                (if (eqv? succ next-label)
                    '(nop)
                    `(br ,(lookup-label succ ctx))))))
            (else
             ;; Otherwise render successor inline.
             (do-tree succ ctx))))
         (define (node-within label ys ctx)
           (pk 'node-within label ys)
           (call-with-values (lambda () (intset-pop ys))
             (lambda (ys y)
               (match y
                 (#f
                  (match (intmap-ref cps label)
                    (($ $kargs names vars term)
                     ;; could change to bind names at continuation?
                     (match term
                       (($ $continue k src exp)
                        (match (intmap-ref cps k)
                          (($ $ktail)
                           (compile-tail exp))
                          (($ $kargs _ vars)
                           `(begin
                              ,(compile-values exp)
                              ,@(reverse (map local.set vars))
                              ,(do-branch label k ctx)))
                          (($ $kreceive ($ $arity req () rest () #f) kargs)
                           (compile-receive exp req rest kargs))))
                       (($ $branch kf kt src op param args)
                        `(if ,(compile-test op param args)
                             (then
                              ,(do-branch label kt (cons 'if-then-else ctx)))
                             (else
                              ,(do-branch label kf (cons 'if-then-else ctx)))))
                       (($ $switch kf kt* src arg)
                        (error "switch unimplemented"))
                       (($ $prompt k kh src escape? tag)
                        (error "prompts should be removed by tailification?"))
                       (($ $throw src op param args)
                        (error "throw unimplemented"))))
                    (($ $kreceive ($ $arity req () rest () #f) kbody)
                     (error "kreceive unimplemented"))
                    (($ $kfun src meta self ktail kentry)
                     (if self
                         ;; only if referenced?
                         `(begin
                            (,@(local.set self) ,(arg-ref 0))
                            ,(do-branch label kentry ctx))
                         (do-tree kentry ctx)))
                    (($ $kclause ($ $arity req opt rest kw allow-other-keys?)
                        kbody kalt)
                     (when kalt (error "case-lambda unimplemented"))
                     (when allow-other-keys? (error "allow-other-keys? unimplemented"))
                     (when (not (null? kw)) (error "kwargs unimplemented"))
                     (when (not (null? opt)) (error "optargs unimplemented"))
                     (when rest (error "rest args unimplemented"))
                     `(if (i32.eq (local.get $nargs) (i32.const ,(1+ (length req))))
                          (then ,@(map (lambda (arg idx)
                                         `(,@(local.set arg) ,(arg-ref (1+ idx))))
                                       (iota (length req)))
                                ,(do-branch label kbody ctx))
                          (else (unreachable))))
                    (($ $ktail)
                     '(nop))))
                 (y
                  `(begin
                     (block ,(node-within label ys (push-block label ctx)))
                     ,(do-tree y ctx)))))))
         (define (sanitize-one x)
           (match x
             (('begin expr ...)
              (match (sanitize-sequence expr)
                (() '(nop))
                ((expr) expr)
                (exprs `(begin . ,exprs))))
             (('if test ... ('then t ...) ('else f ...))
              `(if ,@(sanitize-sequence test)
                   (then ,@(sanitize-sequence t))
                   (else ,@(sanitize-sequence f))))
             (('block expr ...) `(block ,@(sanitize-sequence expr)))
             (('loop expr ...) `(loop ,@(sanitize-sequence expr)))
             (expr expr)))
         (define (sanitize-sequence xs)
           (match xs
             (() '())
             ((('nop) . xs) (sanitize-sequence xs))
             ((('begin . body) . xs)
              (sanitize-sequence (append body xs)))
             ((x . xs)
              (cons (sanitize-one x)
                    (sanitize-sequence xs)))))
         (define code (sanitize-one (do-tree kfun (make-ctx #f '()))))
         `(func (param $nargs i32)
                (param $arg0 (ref eq))
                (param $arg1 (ref eq))
                (param $arg2 (ref eq))
                ,code)))
     (compute-reachable-functions cps 0)))
  (intmap-fold (lambda (kfun fun) (pretty-print fun) (values)) funcs)
  funcs)

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
      (let ((wasm (lower-to-wasm cps)))
        wasm
        #;
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
