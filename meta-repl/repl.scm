(let ()
  (define-foreign document-body
    "document" "body"
    -> (ref extern))
  (define-foreign get-element-by-id
    "document" "getElementById"
    (ref string) -> (ref null extern))
  (define-foreign make-text-node
    "document" "createTextNode"
    (ref string) -> (ref extern))
  (define-foreign make-element
    "document" "createElement"
    (ref string) -> (ref extern))
  (define-foreign make-tree-walker
    "document" "createTreeWalker"
    (ref extern) -> (ref extern))

  (define-foreign prevent-default!
    "event" "preventDefault"
    (ref extern) -> none)
  (define-foreign keyboard-event-code
    "event" "keyboardCode"
    (ref extern) -> (ref string))

  (define-foreign element-value
    "element" "value"
    (ref extern) -> (ref string))
  (define-foreign set-element-value!
    "element" "setValue"
    (ref extern) (ref string) -> none)
  (define-foreign scroll-height
    "element" "scrollHeight"
    (ref extern) -> f64)
  (define-foreign set-scroll-top!
    "element" "setScrollTop"
    (ref extern) f64 -> none)
  (define-foreign append-child!
    "element" "appendChild"
    (ref extern) (ref extern) -> (ref extern))
  (define-foreign remove!
    "element" "remove"
    (ref extern) -> none)
  (define-foreign replace-with!
    "element" "replaceWith"
    (ref extern) (ref extern) -> none)
  (define-foreign set-attribute!
    "element" "setAttribute"
    (ref extern) (ref string) (ref string) -> none)
  (define-foreign remove-attribute!
    "element" "removeAttribute"
    (ref extern) (ref string) -> none)
  (define-foreign add-event-listener!
    "element" "addEventListener"
    (ref extern) (ref string) (ref extern) -> none)
  (define-foreign remove-event-listener!
    "element" "removeEventListener"
    (ref extern) (ref string) (ref extern) -> none)

  (define-foreign current-node
    "treeWalker" "currentNode"
    (ref extern) -> (ref extern))
  (define-foreign set-current-node!
    "treeWalker" "setCurrentNode"
    (ref extern) (ref extern) -> (ref extern))
  (define-foreign next-node!
    "treeWalker" "nextNode"
    (ref extern) -> (ref null extern))
  (define-foreign first-child!
    "treeWalker" "firstChild"
    (ref extern) -> (ref null extern))
  (define-foreign next-sibling!
    "treeWalker" "nextSibling"
    (ref extern) -> (ref null extern))

  (define listeners (make-weak-key-hash-table))
  (define (wrap-listener proc)
    (or (hashq-ref listeners proc)
        (let ((f (procedure->host-function proc)))
          (hashq-set! listeners proc f)
          f)))
  ;; Not the world's best virtual DOM implementation but it will do.
  ;; Assumes that the real DOM's tree structure is the same shape as
  ;; the old virtual DOM and has not been modified elsewhere.
  (define (virtual-dom-render! root old new)
    (define (sxml->dom exp)
      (match exp
        ((? string? str)
         (make-text-node str))
        (((? symbol? tag) . children)
         (let ((elem (make-element (symbol->string tag))))
           (define (add-children children)
             (for-each (lambda (child)
                         (append-child! elem (sxml->dom child)))
                       children))
           (match children
             ((('@ . attrs) . children)
              (for-each (lambda (attr)
                          (match attr
                            (((? symbol? name) (? string? val))
                             (set-attribute! elem
                                             (symbol->string name)
                                             val))
                            (((? symbol? name) (? procedure? proc))
                             (add-event-listener! elem
                                                  (symbol->string name)
                                                  (wrap-listener proc)))))
                        attrs)
              (add-children children))
             (_ (add-children children)))
           elem))))
    (define (attrs+children exp)
      (match exp
        ((('@ . attrs) . children)
         (values attrs children))
        (children
         (values '() children))))
    (define (find-attr attrs name)
      (match attrs
        (() #f)
        ((attr . rest)
         (match attr
           ((name* val)
            (if (eq? name name*)
                val
                (find-attr rest name)))))))
    (define (update-attrs node old-attrs new-attrs)
      (for-each (lambda (attr)
                  (match attr
                    ((name val)
                     (let ((name-str (symbol->string name)))
                       (match (find-attr old-attrs name)
                         (#f
                          (match val
                            ((? string?)
                             (set-attribute! node name-str val))
                            ((? procedure?)
                             (add-event-listener! node name-str
                                                  (wrap-listener val)))))
                         (old-val
                          (match val
                            ((? string?)
                             (unless (string-=? old-val val)
                               (set-attribute! node name-str val)))
                            ((? procedure?)
                             (unless (eq? old-val val)
                               (remove-event-listener! node name-str
                                                       (wrap-listener old-val))
                               (add-event-listener! node name-str
                                                    (wrap-listener val)))))))))))
                new-attrs)
      ;; Delete old attrs that aren't in new.
      (for-each (lambda (attr)
                  (match attr
                    ((name val)
                     (let ((name-str (symbol->string name)))
                       (match (find-attr new-attrs name)
                         (#f
                          (match val
                            ((? string?)
                             (remove-attribute! node name-str))
                            ((? procedure?)
                             (remove-event-listener! node name-str
                                                     (wrap-listener val)))))
                         (_ #t))))))
                old-attrs))
    (let ((walker (make-tree-walker root)))
      (first-child! walker)
      (let loop ((parent root)
                 (old old)
                 (new new))
        (match old
          (#f
           ;; It's the first render, so clear out whatever might be in
           ;; the actual DOM and splat our tree in.
           (let loop ((node (current-node walker)))
             (when node
               (let ((next (next-sibling! walker)))
                 (remove! node)
                 (loop next))))
           (append-child! parent (sxml->dom new)))
          ((? string?)
           ;; Maybe replace text node with either a new text node or
           ;; an element subtree.
           (unless (and (string? new) (string-=? old new))
             (let ((new-node (sxml->dom new)))
               (replace-with! (current-node walker) new-node)
               (set-current-node! walker new-node))))
          (((? symbol? old-tag) . old-rest)
           (let-values (((old-attrs old-children)
                         (attrs+children old-rest)))
             (match new
               ((? string?)
                ;; Replace element subtree with text node.
                (let ((new-text (make-text-node new)))
                  (replace-with! (current-node walker) new-text)
                  (set-current-node! walker new-text)))
               (((? symbol? new-tag) . new-rest)
                (let-values (((new-attrs new-children)
                              (attrs+children new-rest)))
                  (cond
                   ;; Same tag, modify element if necessary.
                   ((eq? old-tag new-tag)
                    (let ((parent (current-node walker)))
                      (update-attrs parent old-attrs new-attrs)
                      (first-child! walker)
                      (let child-loop ((old old-children)
                                       (new new-children))
                        (match old
                          (()
                           ;; Splat in the remaining new children.
                           (for-each (lambda (new)
                                       (append-child! parent (sxml->dom new)))
                                     new))
                          ((old-child . old-rest)
                           (match new
                             ;; Remove all children including the current one.
                             (()
                              (let rem-loop ((node (current-node walker)))
                                (when node
                                  (let ((next (next-sibling! walker)))
                                    (remove! node)
                                    (rem-loop next)))))
                             ;; Diff old and new.
                             ((new-child . new-rest)
                              (loop parent old-child new-child)
                              (next-sibling! walker)
                              (child-loop old-rest new-rest))))))
                      (set-current-node! walker parent)))
                   ;; Different tag, replace entire subtree.
                   (else
                    (replace-with! (current-node walker)
                                   (sxml->dom new)))))))))))))
  (define *current-vdom* #f)
  (define (refresh!)
    (let ((new-vdom (render)))
      (virtual-dom-render! (document-body) *current-vdom* new-vdom)
      (set! *current-vdom* new-vdom)))

  ;; Evaluator
  (define (env-lookup env name)
    (match (assq name env)
      ((_key . val)
       val)
      (_
       (error "Variable unbound:" name))))
  (define (extend-env env names vals)
    (if (eq? names '())
        env
        (cons (cons (car names) (car vals))
              (extend-env env (cdr names) (cdr vals)))))
  (define (evaluate expr env)
    (match expr
      ;; Support builtin types
      ((or #t #f (? number?) (? string?))
       expr)
      ;; Quoting
      (('quote quoted-expr)
       quoted-expr)
      ;; Variable lookup
      ((? symbol? name)
       (env-lookup env name))
      ;; Conditionals
      (('if test consequent alternate)
       (if (evaluate test env)
           (evaluate consequent env)
           (evaluate alternate env)))
      ;; Lambdas (Procedures)
      (('lambda args body)
       (lambda (. vals)
         (evaluate body (extend-env env args vals))))
      ;; Procedure Invocation (Application)
      ((proc-expr . arg-exprs)
       (apply (evaluate proc-expr env)
              (map (lambda (arg-expr)
                     (evaluate arg-expr env))
                   arg-exprs)))))

  ;; Reader
  (define (whitespace-char? char)
    (or (eq? char #\space)
        (eq? char #\tab)
        (eq? char #\newline)))
  (define *numbers*
    (string->list "0123456789"))
  (define *alphabet*
    (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (define *other-symbol-chars*
    (string->list "-_/*+!?%^&="))
  (define (number-char? char)
    (and (member char *numbers*) #t))
  (define (alphabet-char? char)
    (and (member char *alphabet*) #t))
  (define (symbol-char? char)
    (or (alphabet-char? char)
        (number-char? char)
        (and (member char *other-symbol-chars*) #t)))
  (define (simple-read ip)
    (define (consume-whitespace)
      (let lp ()
        (when (whitespace-char? (peek-char ip))
          (read-char ip)
          (lp))))
    (define (read-list)
      (let lp ()
        (consume-whitespace)
        (cond
         ((eq? (peek-char ip) #\))
          (read-char ip)
          '())
         (else
          (let ((next-item (read-expr)))
            (cons next-item (lp)))))))
    (define (read-string)
      (list->string
       (let lp ((escape? #f))
         (let ((c (peek-char ip)))
           (cond
            ((and (eqv? c #\") (not escape?))
             (read-char ip)
             '())
            ((and (eqv? c #\\) (not escape?))
             (read-char ip)
             (lp #t))
            ((and (eqv? c #\n) escape?)
             (read-char ip)
             (cons #\newline (lp #f)))
            (else
             (cons (read-char ip) (lp #f))))))))
    (define (read-symbol)
      (string->symbol
       (list->string
        (let lp ()
          (let ((c (peek-char ip)))
            (cond
             ((or (whitespace-char? c) (eq? c #\)) (eof-object? c))
              '())
             ((symbol-char? c)
              (cons (read-char ip) (lp)))
             (else (error "Unexpected symbol while parsing symbol:" c))))))))
    ;; not the best implementation but, uh, it'll work for now
    (define (read-number)
      (string->number
       (list->string
        (let lp ()
          (let ((c (peek-char ip)))
            (if (or (number-char? c) (eq? c #\.))
                (cons (read-char ip) (lp))
                '()))))))
    (define (read-expr)
      (consume-whitespace)
      (let ((c (peek-char ip)))
        (cond
         ((eof-object? c)
          (error "End of file while reading expression"))
         ((eqv? c #\()
          (read-char ip)
          (read-list))
         ((eqv? c #\')
          (read-char ip)
          (list 'quote (read-expr)))
         ((eqv? c #\`)
          (read-char ip)
          (list 'quasiquote (read-expr)))
         ((eqv? c #\,)
          (read-char ip)
          (list 'unquote (read-expr)))
         ((eqv? c #\-)
          (read-char ip)
          (* -1 (read-number)))
         ((eqv? c #\")
          (read-char ip)
          (read-string))
         ((number-char? c)
          (read-number))
         ((symbol-char? c)
          (read-symbol))
         ((eq? c #\#)
          (read-char ip)
          (let ((c (peek-char ip)))
            (cond
             ((eq? c #\f) (read-char ip) #f)
             ((eq? c #\t) (read-char ip) #t)
             ((eq? c #\() (read-char ip) (list->vector (read-list))))))
         (else
          (error "Syntax error")))))
    (read-expr))

  (define *log*
    '("Welcome to the Hoot metacircular evaluator demo!

This is a simple Scheme interpreter written in Scheme, compiled to
WebAssembly, and running in the web browser. The UI rendering code is
also written in Scheme and uses the Hoot FFI to render to the DOM.

"))

  (define (log-append! . lines)
    (set! *log* (append *log* lines)))

  (define %prompt "> ")

  (define (repl-pk . vals)
    (display ";;; ")
    (write vals)
    (newline))

  (define *unspecified* (if #f #f))
  (define (unspecified? x)
    (eq? x *unspecified*))

  (define init-env
    `((+ . ,+)
      (- . ,-)
      (* . ,*)
      (/ . ,/)
      (= . ,=)
      (values . ,values)
      (display . ,display)
      (newline . ,newline)
      (pk . ,repl-pk)))

  (define (eval! str)
    (let ((exp (simple-read (open-input-string str)))
          (output (open-output-string)))
      (parameterize ((current-output-port output))
        (display %prompt)
        (display str)
        (newline)
        (call-with-values (lambda () (evaluate exp init-env))
          (lambda vals
            (if (null? vals)
                (display "\n")
                (for-each (lambda (val)
                            (unless (unspecified? val)
                              (display "=> ")
                              (write val))
                            (newline))
                          vals)))))
      (log-append! (get-output-string output))))

  (define (scroll-to-bottom!)
    (let ((repl (get-element-by-id "repl")))
      (set-scroll-top! repl (scroll-height repl))))

  (define (maybe-eval event)
    (let ((key (keyboard-event-code event)))
      (when (string-=? key "Enter")
        (let* ((input (get-element-by-id "expression"))
               (exp (element-value input)))
          (unless (string-=? exp "")
            (set-element-value! input "")
            (eval! exp)
            (refresh!)
            (scroll-to-bottom!))))))

  (define (render)
    (define (render-log)
      (map (lambda (line)
             `(p ,line))
           *log*))
    `(div (@ (class "container"))
      (h1 "Hoot REPL")
      (div (@ (id "repl")
              (class "repl repl-text"))
           (div (@ (class "log"))
                ,@*log*)
           (div (@ (class "prompt"))
                ,%prompt
                (input (@ (id "expression")
                          (class "repl-text")
                          (placeholder "(+ 1 2 3)")
                          (keyup ,maybe-eval)))))))

  (refresh!))
