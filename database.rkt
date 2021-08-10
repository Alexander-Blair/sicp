#lang sicp

(#%require (only racket/base error))
(#%require (only racket/base make-hash))
(#%require (only racket/base hash-set!))
(#%require (only racket/base hash-ref))
(#%require (only "get-and-put.rkt" get put))
(#%require (only "streams.rkt"
                 the-empty-stream
                 stream-car
                 stream-cdr
                 stream-append
                 stream-append-delayed
                 stream-map
                 singleton-stream
                 singleton-stream?
                 stream-flatmap
                 interleave-delayed
                 stream-for-each
                 stream-null?
                 cons-stream
                 delay
                 stream-filter))

(define user-initial-environment (scheme-report-environment 5))

(define input-prompt  ";;; Query input:")
(define output-prompt ";;; Query results:")


(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! 
             (add-assertion-body q))
           (newline)
           (display 
             "Assertion added to data base.")
           (query-driver-loop))
          (else
            (newline)
            (display output-prompt)
            (let ((result (qeval q (singleton-stream '()))))
              (display-stream
                (stream-map
                  (lambda (frame)
                    (instantiate
                      q
                      frame
                      (lambda (v f)
                        (contract-question-mark v))))
                  result)))
            (query-driver-loop)))))


(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))


(define (display-stream s)
  (stream-for-each display-line s))


(define (display-line x)
  (newline)
  (display x))


(define (instantiate 
         exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler 
                  exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) 
                 (copy (cdr exp))))
          (else exp)))
  (copy exp))


; When instantiating a lisp-value expression, we need a way to signal back to the
; caller that we could not bind all the variables. This is not possible using the
; `instantiate` function, as the unbound-var-handler can only either replace the
; unbound var with a different value, or use an exception to abort the function.
(define (instantiate-lisp-expression exp frame)
  (define (iter result exp)
    (cond ((null? exp) (reverse result))
          ((var? (car exp))
           (let ((binding (binding-in-frame (car exp) frame)))
             (if binding
                 (iter (cons (binding-value binding) result)
                       (cdr exp))
                 false)))
          (else (iter (cons (car exp) result)
                      (cdr exp)))))
  (iter '() exp))


(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if (not (null? qproc))
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))


(define (combine-frames-if-consistent f1 f2)
  (cond ((eq? f1 'failed) 'failed)
        ((eq? f2 'failed) 'failed)
        ((null? f2) f1)
        (else
          (combine-frames-if-consistent (extend-if-consistent (caar f2) (cdar f2) f1)
                                        (cdr f2)))))


(define (simple-query query-pattern 
                      frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (unresolved-promise? frame)
          (resolve-promise frame)
          (singleton-stream frame)))
    (stream-flatmap
      (lambda (frame)
        (stream-append-delayed
          (find-assertions query-pattern frame)
          (delay 
            (apply-rules query-pattern frame))))
      frame-stream)))


(define (includes-lisp-value-clause? conjuncts)
  (cond ((null? conjuncts) false)
        ((eq? (caar conjuncts) 'lisp-value) true)
        (else (includes-lisp-value-clause? (cdr conjuncts)))))


(define (slow-conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (slow-conjoin (rest-conjuncts conjuncts)
                    (qeval
                      (first-conjunct conjuncts)
                      frame-stream))))


(define (fast-conjoin conjuncts frame-stream)
  (define (iter combined-stream evaluated-conjuncts)
    (if (null? evaluated-conjuncts)
        combined-stream
        (iter (extend-consistent-frames combined-stream (car evaluated-conjuncts))
              (cdr evaluated-conjuncts))))
  (iter (singleton-stream '())
        (map (lambda (conjunct)
               (qeval conjunct frame-stream))
             conjuncts)))


(define (extend-consistent-frames frame-stream-1 frame-stream-2)
  (define (frame-match? frame)
    (not (eq? frame 'failed)))
  (if (stream-null? frame-stream-1)
      frame-stream-2
      (stream-flatmap (lambda (f1)
                        (stream-filter
                          frame-match?
                          (stream-flatmap
                            (lambda (f2)
                              (let ((combined (combine-frames-if-consistent f1 f2)))
                                (cond ((eq? combined 'failed)
                                       (singleton-stream combined))
                                      ((unresolved-promise? combined)
                                       (resolve-promise combined))
                                      (else
                                        (singleton-stream combined)))))
                            frame-stream-2)))
                      frame-stream-1)))


(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) 
              frame-stream)
       (delay (disjoin 
               (rest-disjuncts disjuncts)
               frame-stream)))))


(define (all-vars-bound? exp frame)
  (cond ((null? exp) true)
        ((var? (car exp))
         (if (binding-in-frame (car exp) frame)
             (all-vars-bound? (cdr exp) frame)
             false))
        (else (all-vars-bound? (cdr exp) frame))))


(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((query (negated-query operands)))
        (if (all-vars-bound? query frame)
            (if (stream-null? (qeval query (singleton-stream frame)))
                (singleton-stream frame)
                the-empty-stream)
            (singleton-stream (extend-frame-with-promise (list 'not query) frame)))))
    frame-stream))


(define (uniquely-asserted contents frame-stream)
  (define (filter-unique frame)
    (let ((extended-frames (qeval (car contents)
                                  (singleton-stream frame))))
      (if (singleton-stream? extended-frames)
          extended-frames
          the-empty-stream)))
  (if (stream-null? frame-stream)
      (filter-unique frame-stream)
      (stream-flatmap filter-unique frame-stream)))


(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((resolved-expression (instantiate-lisp-expression call frame)))
        (if resolved-expression
            (if (execute resolved-expression)
                (singleton-stream frame)
                the-empty-stream)
            (singleton-stream (extend-frame-with-promise (cons 'lisp-value call) frame)))))
    frame-stream))


(define (resolve-promise frame)
  (qeval (binding-value (binding-in-frame 'promise frame))
         (singleton-stream (unbind 'promise frame))))


(define (unresolved-promise? frame)
  (binding-in-frame 'promise frame))


(define (extend-frame-with-promise promise frame)
  (extend 'promise promise frame))


(define (simple-lisp-value call frame-stream)
  (simple-stream-flatmap
   (lambda (frame)
     (if (execute (instantiate-lisp-expression call frame))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))


(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))


(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (f) (not (stream-null? f)))
                             stream)))


(define (execute exp)
  (apply (eval (predicate exp) 
               user-initial-environment)
         (args exp)))


(define (always-true ignore frame-stream) 
  frame-stream)


(define (find-assertions pattern frame)
  (stream-flatmap 
    (lambda (datum) 
      (check-an-assertion datum pattern frame))
    (fetch-assertions pattern)))


(define (check-an-assertion 
         assertion query-pat query-frame)
  (let ((match-result
         (pattern-match 
          query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))


(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) 
         (extend-if-consistent 
          pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match 
          (cdr pat) 
          (cdr dat)
          (pattern-match
           (car pat) (car dat) frame)))
        (else 'failed)))


(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match 
         (binding-value binding) dat frame)
        (extend var dat frame))))


(define (apply-rules pattern frame)
  (stream-flatmap 
   (lambda (rule)
     (apply-a-rule rule pattern frame))
   (fetch-rules pattern)))


(define (apply-a-rule rule
                      query-pattern
                      query-frame)
  (let ((clean-rule 
         (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                        (conclusion clean-rule)
                        query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream 
                  unify-result))))))


(define (rename-variables-in rule)
  (let ((rule-application-id 
         (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable 
              exp 
              rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))


(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1)
         (extend-if-possible p1 p2 frame))
        ((var? p2)
         (extend-if-possible 
          p2 
          p1 
          frame))        ; ***
        ((and (pair? p1) 
              (pair? p2))
         (unify-match 
          (cdr p1) 
          (cdr p2)
          (unify-match 
           (car p1)
           (car p2)
           frame)))
        (else 'failed)))


(define (extend-if-possible var val frame)
  (let ((binding (binding-in-frame var frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)                   ; ***
           (let ((binding 
                  (binding-in-frame 
                   val
                   frame)))
             (if binding
                 (unify-match
                  var 
                  (binding-value binding) 
                  frame)
                 (extend var val frame))))
          ((depends-on? val var frame)  ; ***
           'failed)
          (else (extend var val frame)))))


(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? var e)
               true
               (let
                 ((b (binding-in-frame 
                      e 
                      frame)))
                  (if b
                      (tree-walk 
                       (binding-value b))
                      false))))
          ((pair? e)
           (or (tree-walk (car e))
               (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))


(define THE-ASSERTIONS the-empty-stream)


(define (fetch-assertions pattern)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))


(define (get-all-assertions) THE-ASSERTIONS)


(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern)
              'assertion-stream))


(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))


(define THE-RULES the-empty-stream)


(define (fetch-rules pattern)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))


(define (get-all-rules) THE-RULES)


(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern)
               'rule-stream)
   (get-stream '? 'rule-stream)))


(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))


(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion 
                       old-assertions))
    'ok))


(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES
          (cons-stream rule old-rules))
    'ok))


(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream 
                key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream 
                assertion
                current-assertion-stream))))))


(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream 
                  key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream 
                  rule
                  current-rule-stream)))))))


(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))


(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))


(define (use-index? pat)
  (constant-symbol? (car pat)))


(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression TYPE" exp)))


(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression CONTENTS" exp)))


(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))


(define (add-assertion-body exp)
  (car (contents exp)))


(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))
(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))
(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))


(define (rule? statement)
  (tagged-list? statement 'rule))


(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


(define (conclusion rule) (cadr rule))


(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))


(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))


(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols 
                proc (car exp))
               (map-over-symbols 
                proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))


(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '? (string->symbol
                  (substring
                   chars 
                   1 
                   (string-length chars))))
        symbol)))


(define (var? exp) (tagged-list? exp '?))
(define (constant-symbol? exp) (symbol? exp))


(define rule-counter 0)


(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)


(define (make-new-variable 
         var rule-application-id)
  (cons '? (cons rule-application-id
                 (cdr var))))


(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
     (if (number? (cadr variable))
         (string-append
          (symbol->string (caddr variable))
          "-"
          (number->string (cadr variable)))
         (symbol->string (cadr variable))))))


(define (make-binding variable value)
  (cons variable value))


(define (binding-variable binding)
  (car binding))


(define (binding-value binding)
  (cdr binding))


(define (binding-in-frame variable frame)
  (assoc variable frame))


(define (extend variable value frame)
  (cons (make-binding variable value) frame))


(define (unbind variable frame)
  (define (iter f)
    (cond ((null? f) '())
          ((equal? (caar f) variable)
           (iter (cdr f)))
          (else (cons (car f) (iter (cdr f))))))
  (iter frame))


(define initial-assertions '(
  (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
  (job (Bitdiddle Ben) (computer wizard))
  (salary (Bitdiddle Ben) 60000)
  (address (Hacker Alyssa P) 
           (Cambridge (Mass Ave) 78))
  (job (Hacker Alyssa P) (computer programmer))
  (salary (Hacker Alyssa P) 40000)
  (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
  
  (address (Fect Cy D) 
           (Cambridge (Ames Street) 3))
  (job (Fect Cy D) (computer programmer))
  (salary (Fect Cy D) 35000)
  (supervisor (Fect Cy D) (Bitdiddle Ben))
  
  (address (Tweakit Lem E) 
           (Boston (Bay State Road) 22))
  (job (Tweakit Lem E) (computer technician))
  (salary (Tweakit Lem E) 25000)
  (supervisor (Tweakit Lem E) (Bitdiddle Ben))
  (address (Reasoner Louis) 
           (Slumerville (Pine Tree Road) 80))
  (job (Reasoner Louis) 
       (computer programmer trainee))
  (salary (Reasoner Louis) 30000)
  (supervisor (Reasoner Louis) 
              (Hacker Alyssa P))
  (supervisor (Bitdiddle Ben) (Warbucks Oliver))
  (address (Warbucks Oliver) 
           (Swellesley (Top Heap Road)))
  (job (Warbucks Oliver) 
       (administration big wheel))
  (salary (Warbucks Oliver) 150000)
  (address (Scrooge Eben) 
         (Weston (Shady Lane) 10))
  (job (Scrooge Eben) 
       (accounting chief accountant))
  (salary (Scrooge Eben) 75000)
  (supervisor (Scrooge Eben) (Warbucks Oliver))
  
  (address (Cratchet Robert) 
           (Allston (N Harvard Street) 16))
  (job (Cratchet Robert) (accounting scrivener))
  (salary (Cratchet Robert) 18000)
  (supervisor (Cratchet Robert) (Scrooge Eben))
  (address (Aull DeWitt) 
           (Slumerville (Onion Square) 5))
  (job (Aull DeWitt) (administration secretary))
  (salary (Aull DeWitt) 25000)
  (supervisor (Aull DeWitt) (Warbucks Oliver))
  ))

(define (add-assertions assertions)
  (if (null? assertions)
      'done
      (begin
        (add-assertion! (car assertions))
        (add-assertions (cdr assertions)))))

(put 'and 'qeval fast-conjoin)
(put 'or 'qeval disjoin)
(put 'not 'qeval negate)
(put 'lisp-value 'qeval lisp-value)
(put 'simple-lisp-value 'qeval simple-lisp-value)
(put 'always-true 'qeval always-true)
(put 'unique 'qeval uniquely-asserted)

(add-assertions initial-assertions)

(define frame-1 '(((? role) . technician) ((? name) Tweakit Lem E)))
(define frame-2 '(((? name) Tweakit Lem E) ((? amount) . 25000)))
(define frame-3 '(((? name) Blobby Mr) ((? amount) . 25000)))
'(assert! (rule (rich ?who) (and (salary ?who ?amount) (lisp-value > ?amount 40000))))
'(assert! (rule (richer-than ?person-1 ?person-2) (and (salary ?person-1 ?amount-1) (salary ?person-2 ?amount-2) (lisp-value > ?amount-1 ?amount-2))))
'(and (job ?person (computer ?role)) (salary ?person ?amount))
'(and (job ?person (computer ?role)) (not (job ?person (computer wizard))))
'(and (not (job ?person (computer wizard))) (job ?person (computer ?role)))
'(and (job ?person (computer ?role)) (lisp-value > ?amount 30000) (salary ?person ?amount))

(define (qdl) (query-driver-loop))
