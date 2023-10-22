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

  (define-foreign element-value
    "element" "value"
    (ref extern) -> (ref string))

  (define-foreign set-element-value!
    "element" "setValue"
    (ref extern) (ref string) -> none)

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
                             (remove-event-listener! node name-str val))))
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
               (let ((next (next-node! walker)))
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

  (define *id-counter* 0)

  (define (fresh-id)
    (let ((id *id-counter*))
      (set! *id-counter* (+ *id-counter* 1))
      id))

  (define *tasks* '())

  (define (make-task id name done?)
    (list id name done?))

  (define (add-task name)
    (set! *tasks* (reverse (cons (make-task (fresh-id) name #f) (reverse *tasks*)))))

  (define (remove-task id)
    (set! *tasks*
          (let loop ((tasks *tasks*))
            (match tasks
              (() '())
              ((task . rest)
               (match task
                 ((id* _ _)
                  (if (= id id*)
                      rest
                      (cons task (loop rest))))))))))

  (define (replace-task id new-task)
    (set! *tasks*
          (let loop ((tasks *tasks*))
            (match tasks
              (() '())
              ((task . rest)
               (match task
                 ((id* _ _)
                  (if (= id id*)
                      (cons new-task rest)
                      (cons task (loop rest))))))))))

  (define (submit . _)
    (let* ((input (get-element-by-id "new-task"))
           (name (element-value input)))
      (unless (string-=? name "")
        (add-task name)
        (set-element-value! input "")
        (refresh!))))

  (define (render)
    (define (render-task task)
      (match task
        ((id name done?)
         `(li (input (@ (type "checkbox")
                        (click ,(lambda _
                                  (replace-task id (make-task id name (not done?)))
                                  (refresh!)))
                        ,@(if done? '((checked "")) '())))
              " " ,(if done? `(s ,name) name) " "
              (a (@ (href "#")
                    (click ,(lambda _
                              (remove-task id)
                              (refresh!))))
                 "remove")))))
    `(div
      (h2 "Tasks")
      (ul ,@(map render-task *tasks*))
      (input (@ (id "new-task") (placeholder "Write more Scheme")))
      (button (@ (click ,submit)) "Add task")))

  (add-task "Walk the dog")
  (add-task "Pet the cats")
  (add-task "Feed the chickens")
  (add-task "Pick up the kids from school")
  (refresh!))
