(use-modules (ice-9 match)
             (ice-9 pretty-print)
             (ice-9 format)
             (ice-9 textual-ports))

(define (make-char-mapper f)
  (define (diff ch)
    (- (char->integer (f ch)) (char->integer ch)))
  `(lambda (ch)
     (let ((cp (char->integer ch)))
       (integer->char
        (+ cp (,(generate-codepoint-lookup-table diff) cp))))))

(define (make-char-upcase) (make-char-mapper char-upcase))
(define (make-char-downcase) (make-char-mapper char-downcase))

(define (generate-codepoint-lookup-table f)
  (define (visit-codepoint-range start end clauses)
    (define (adjoin-span cp val out)
      (match out
        (() (acons cp val '()))
        (((end . val*) . tail)
         (if (eqv? val val*)
             (acons cp val* tail)
             (acons cp val out)))))
    (define (fold-clauses clauses out)
      (match out
        (() clauses)
        (((end . val) (end* . val*) . out)
         (let ((stride (- end end*)))
           (define (finish start out)
             (let* ((span (- end start))
                    (nclauses (/ span stride)))
               (unless (and (exact-integer? nclauses) (positive? nclauses))
                 (error "unexpected" nclauses))
               (fold-clauses
                (append
                 (match nclauses
                   (1 `(((,(if (= span 1) '= '<=) cp ,end) ,val)))
                   (2 `(((<= cp ,end*) ,val*)
                        ((<= cp ,end) ,val)))
                   (_
                    `(((<= cp ,end)
                       (if (logtest
                            1
                            ,(cond
                              ((= 1 (logcount stride))
                               ;; Stride is a power of two.
                               (define (quotient/shift expr shift)
                                 (if (zero? shift)
                                     expr
                                     `(ash ,expr ,shift)))
                               ;; Compute the offset from the start of
                               ;; the span, unless the start is already
                               ;; aligned.
                               (define offset
                                 (if (logtest (1+ start) (1- (ash stride 1)))
                                     `(- cp ,(1+ start))
                                     'cp))
                               (quotient/shift offset
                                               (- (logcount (1- stride)))))
                              (else
                               `(even? (quotient (- cp ,(1+ start)) ,stride)))))
                           ,(if (even? nclauses) val val*)
                           ,(if (even? nclauses) val* val))))))
                 clauses)
                out)))
           (let lp ((prev-end end*) (prev-val val*) (out out)
                    (expected val*) (alternate val))
             (match out
               (()
                (let ((end (1- start)))
                  (if (= end (- prev-end stride))
                      (finish end '())
                      (finish prev-end (acons prev-end prev-val out)))))
               (((end . val) . tail)
                (cond
                 ((and (= end (- prev-end stride))
                       (eqv? prev-val expected))
                  (lp end val tail alternate expected))
                 (else
                  (finish prev-end (acons prev-end prev-val out)))))))))
        (((end . val))
         (cons `((<= cp ,end) ,val) clauses))))
    (let lp ((cp start) (out '()))
      (if (<= cp end)
          (lp (1+ cp) (adjoin-span cp (f (integer->char cp)) out))
          (fold-clauses clauses out))))
  (define* (make-binary-search v #:optional (start 0) (end (vector-length v)))
    (cond
     ((= start end)
      (if (= start (vector-length v))
          `(error "unreachable")
          (match (vector-ref v start)
            ((test expr) expr))))
     (else
      (let ((mid (ash (+ start end) -1)))
        (match (vector-ref v mid)
          ((((or '= '<=) 'cp val) _)
           `(if (<= cp ,val)
                ,(make-binary-search v start mid)
                ,(make-binary-search v (1+ mid) end))))))))
  (let* ((clauses '())
         (clauses (visit-codepoint-range #xe000 #x10ffff clauses))
         (clauses (visit-codepoint-range 0 #xd7ff clauses)))
    `(lambda (cp) ,(make-binary-search (list->vector clauses)))))

(define (test-char-procedure reference proc)
  (char-set-for-each
   (lambda (ch)
     (define expected (reference ch))
     (define actual (proc ch))
     (unless (eqv? expected actual)
       (error "results differ" ch (char->integer ch) expected actual)))
   char-set:full))

(when (batch-mode?)
  (match (program-arguments)
    ((_)
     (define (<< str)
       (put-string (current-output-port) str))
     (define (pp expr)
       (pretty-print expr (current-output-port)))
     (<< ";; This file was generated by generate-char-stdlib.scm.\n\n")
     (pp `(define char-upcase ,(make-char-upcase)))
     (<< "\n")
     (pp `(define char-downcase ,(make-char-downcase))))
    ((arg0 . _)
     (format (current-error-port) "usage: ~a\n" arg0)
     (exit 1))))
