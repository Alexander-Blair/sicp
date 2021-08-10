#lang sicp

(define (apply-in-underlying-scheme proc args)
  (apply proc args))

(define (eval exp env)
  ((analyze exp) env))

(define (actual-value exp env)
    (force-it (eval exp env)))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (quoted-list? obj)
  (and (quoted? obj)
       (list? (cadr obj))))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) 
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result
                (actual-value 
                 (thunk-exp obj)
                 (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           ;; replace exp with its value:
           (set-car! (cdr obj) result) 
           ;; forget unneeded env:
           (set-cdr! (cdr obj) '()) 
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (quoted-list-to-list exp)
  (if (null? exp)
      (list)
      (cons 'list (map (lambda (x) (list 'quote x))
                       (cadr exp)))))

(define (amb-analyze exp)
  (cond ((self-evaluating? exp)
         (amb-analyze-self-evaluating exp))
        ((quoted-list? exp)
         (amb-analyze (quoted-list-to-list exp)))
        ((quoted? exp) 
         (amb-analyze-quoted exp))
        ((variable? exp) 
         (amb-analyze-variable exp))
        ((assignment? exp) 
         (amb-analyze-assignment exp))
        ((permanent-assignment? exp) 
         (amb-analyze-permanent-assignment exp))
        ((definition? exp) 
         (amb-analyze-definition exp))
        ((if-fail? exp) 
         (amb-analyze-if-fail exp))
        ((require? exp)
         (amb-analyze-require exp))
        ((if? exp) 
         (amb-analyze-if exp))
        ((unless? exp)
         (amb-analyze (unless->if exp)))
        ((lambda? exp) 
         (amb-analyze-lambda exp))
        ((begin? exp) 
         (amb-analyze-sequence 
          (begin-actions exp)))
        ((cond? exp) 
         (amb-analyze (cond->if exp)))
        ((let? exp)
         (amb-analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((application? exp) 
         (amb-analyze-application exp))
        (else
         (error "Unknown expression type: ANALYZE" exp))))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted-list? exp)
         (analyze (list->lazy-list (cadr exp))))
        ((quoted? exp)
         (analyze-quoted exp))
        ((variable? exp)
         (analyze-variable exp))
        ((assignment? exp) 
         (analyze-assignment exp))
        ((definition? exp) 
         (analyze-definition exp))
        ((if? exp) 
         (analyze-if exp))
        ((unless? exp)
         (analyze (unless->if exp)))
        ((lambda? exp) 
         (analyze-lambda exp))
        ((begin? exp) 
         (analyze-sequence 
          (begin-actions exp)))
        ((cond? exp) 
         (analyze (cond->if exp)))
        ((let? exp)
         (analyze (let->combination exp)))
        ((application? exp) 
         (analyze-application exp))
        (else
         (error "Unknown expression type: ANALYZE" exp))))

(define (require? exp) 
  (tagged-list? exp 'require))

(define (require-predicate exp) 
  (cadr exp))

(define (amb-analyze-require exp)
  (let ((pproc (amb-analyze 
                (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

(define (ramb? exp)
  (tagged-list? exp 'ramb))

(define (ambeval exp env succeed fail)
  ((amb-analyze exp) env succeed fail))

(define (amb-analyze-self-evaluating exp)
  (display "SELF EVALUATING!")
  (newline)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (amb-analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (amb-analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (amb-analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (amb-analyze-sequence 
                (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (amb-analyze-if exp)
  (let ((pproc (amb-analyze (if-predicate exp)))
        (cproc (amb-analyze (if-consequent exp)))
        (aproc (amb-analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating
             ;; the predicate to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for
             ;; evaluating the predicate
             fail))))

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-test exp)
  (cadr exp))

(define (fail-value exp)
  (caddr exp))

(define (amb-analyze-if-fail exp)
  (let ((test (amb-analyze (if-fail-test exp)))
        (catch (amb-analyze (fail-value exp))))
    (lambda (env succeed fail)
      (letrec ((fail2 (lambda () (catch env succeed fail2))))
        (test env
              succeed
              fail2)))))

(define (amb-analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map amb-analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (amb-analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (amb-analyze 
                (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (amb-analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (amb-analyze 
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)    ; *1*
               (let ((old-value
                      (lookup-variable-value 
                       var 
                       env)))
                 (set-variable-value!
                  var 
                  val 
                  env)
                 (succeed 
                  'ok
                  (lambda ()    ; *2*
                    (set-variable-value! 
                     var
                     old-value
                     env)
                    (fail2)))))
             fail))))

(define (amb-analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (amb-analyze 
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value!
                var 
                val 
                env)
               (succeed 
                'ok
                fail2))
             fail))))

(define (amb-analyze-application exp)
  (let ((fproc (amb-analyze (operator exp)))
        (aprocs (map amb-analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args 
                aprocs
                env
                (lambda (args fail3)
                  (amb-execute-application
                   proc args succeed fail3))
                fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) 
       env
       ;; success continuation for this aproc
       (lambda (arg fail2)
         (get-args 
          (cdr aprocs)
          env
          ;; success continuation for
          ;; recursive call to get-args
          (lambda (args fail3)
            (succeed (cons arg args)
                     fail3))
          fail2))
       fail)))

(define (amb-execute-application 
         proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed 
          (apply-primitive-procedure 
           proc args)
          fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment 
           (procedure-parameters proc)
           args
           (procedure-environment proc))
          succeed
          fail))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION"
                     proc))))

(define (analyze-amb exp)
  (let ((cprocs
         (map amb-analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) 
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

; Iterates through items to a random index, after which it creates a list
; where the item at the given index goes to the front of the list. The order
; of the rest of the list is not guaranteed to stay the same
(define (move-random-item-to-front items)
  (define (iter seq seen-items random-i i)
    (if (eq? random-i i)
        (cons (car seq)
              (append seen-items (cdr seq)))
        (iter (cdr seq) (cons (car seq) seen-items) random-i (+ i 1))))
  (if (eq? (length items) 1)
      items
      (iter items '() (random (- (length items) 1)) 0)))

(define (random-order items)
  (if (null? items)
      '()
      (let ((new-items (move-random-item-to-front items)))
        (cons (car new-items) (random-order (cdr new-items))))))

(define (analyze-ramb exp)
  (let ((cprocs
         (map amb-analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((randomise-first (move-random-item-to-front choices)))
              ((car randomise-first)
               env
               succeed
               (lambda ()
                 (try-next (cdr randomise-first)))))))
      (try-next cprocs))))

(define amb-input-prompt  ";;; Amb-Eval input:")
(define amb-output-prompt ";;; Amb-Eval value:")

(define (amb-driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input amb-input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display 
             ";;; Starting a new problem ")
            (ambeval 
             input
             the-global-environment
             ;; ambeval success
             (lambda (val next-alternative)
               (announce-output 
                amb-output-prompt)
               (user-print val)
               (internal-loop 
                next-alternative))
             ;; ambeval failure
             (lambda ()
               (announce-output
                ";;; There are no 
                 more values of")
               (user-print input)
               (amb-driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display 
      ";;; There is no current problem")
     (amb-driver-loop))))

(define (let? exp)
  (tagged-list? exp 'let))

(define (unless? exp)
  (tagged-list? exp 'unless))

(define (unless->if exp)
  (append (list 'if (list 'not (cadr exp))) (cddr exp)))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) 
    (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env)
      (set-variable-value! 
       var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze 
                (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence 
                (lambda-body exp))))
    (lambda (env)
      (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc 
                            (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application 
       (fproc env)
       (map (lambda (aproc) (aproc env))
            aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment 
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else (error "Unknown procedure type: EXECUTE-APPLICATION"
                     proc))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value 
             (first-operand exps) 
             env)
            (list-of-arg-values 
             (rest-operands exps)
             env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it 
             (first-operand exps) 
             env)
            (list-of-delayed-args 
             (rest-operands exps)
             env))))

(define (make-named-let name declarations body)
  (list 'let
        name
        declarations
        body))

(define (make-let declarations body)
  (list 'let declarations body))

(define (while-predicate expr) (cadr expr)) 
(define (while-procedure expr) (caddr expr)) 

(define (while->combination expr)
  (make-named-let 'iter
                  '()
                  (make-if (list (while-predicate expr))
                           (sequence->exp (list (list (while-procedure expr))
                                                (list 'iter)))
                           'true)))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values 
             (rest-operands exps) 
             env))))

(define (list-of-values-lr exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-operand exps) env)))
        (let ((right (list-of-values-lr (rest-operands exps) env)))
          (cons left right)))))

(define (list-of-values-rl exps env)
  (if (no-operands? exps)
      '()
      (let ((right (eval (first-operand exps) env)))
        (let ((left (list-of-values-rl (rest-operands exps) env)))
          (cons left right)))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) 
         (eval (first-exp exps) env))
        (else 
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) 
                        env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((or (compound-procedure? exp)
             (primitive-procedure? exp)) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

(define (list->lazy-list l)
  (if (null? l)
      'nil
      (list 'cons (list 'quote (car l)) (list->lazy-list (cdr l)))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (cond ((symbol? (cadr exp)) (caddr exp))
        ((list? (cadr exp))
         (make-lambda
          (cdadr exp)   ; formal parameters
          (cddr exp))) ; body
        ((pair? (cadr exp))
         (make-lambda
          (list 'splat (cdadr exp))
          (cddr exp)))))

(define (lambda? exp) 
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp)) 
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if 
        predicate 
        consequent 
        alternative))

(define (cond? exp) 
  (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-arrow-procedure? cond-actions)
  (eq? (car cond-actions) '=>))
(define (cond-arrow-procedure cond-actions)
  (cadr cond-actions))
(define (cond-predicate clause) 
  (car clause))
(define (cond-actions clause) 
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                 (cond-actions first))
                (error "ELSE clause isn't last: COND->IF" clauses))
            (make-if (cond-predicate first)
                     (if (cond-arrow-procedure? (cond-actions first))
                         ((cond-arrow-procedure (cond-actions first)) (cond-predicate first))
                         (sequence->exp
                          (cond-actions first)))
                     (expand-clauses 
                      rest))))))

(define (begin? exp) 
  (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (and? exp)
  (tagged-list? exp 'and))
(define (first-and-predicate exps)
  (car exps))
(define (rest-and-predicates exps)
  (cdr exps))
(define (application? exp) (pair? exp))
(define (eval-and-predicates exp env)
  (cond ((no-operands? exp) true)
        ((true? (eval (first-and-predicate exp)))
         (eval-and-predicates (rest-and-predicates exp) env))
        (else false)))

(define (or? exp)
  (tagged-list? exp 'or))
(define (first-or-predicate exps)
  (car exps))
(define (rest-or-predicates exps)
  (cdr exps))
(define (eval-or-predicates exp env)
  (cond ((no-operands? exp) false)
        ((true? (eval (first-or-predicate exp)))
         true)
        (else (eval-or-predicates (rest-or-predicates exp) env))))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (let-declarations exp)
  (cadr exp))

(define (let-parameters declarations)
  (if (null? declarations)
      '()
      (cons (caar declarations)
            (let-parameters (cdr declarations)))))

(define (let-arguments declarations)
  (if (null? declarations)
      '()
      (cons (cadar declarations)
            (let-arguments (cdr declarations)))))

(define (let->combination exp)
  (cons (make-lambda (let-parameters (let-declarations exp))
                     (let-body exp))
        (let-arguments (let-declarations exp))))

(define (let-body exp)
  (cddr exp))

(define (let*->nested-lets exp)
  (expand-let*-declarations (let-declarations exp) (let-body exp)))
   
(define (expand-let*-declarations declarations body)
  (if (null? declarations)
      body
      (make-let (list (car declarations))
                (expand-let*-declarations (cdr declarations) body))))

(define (letrec->let exp)
  (let ((unassigned-declarations (map (lambda (parameter)
                                        (list parameter "*unassigned"))
                                      (let-parameters (let-declarations exp))))
        (assignments (map (lambda (declaration)
                            (list 'set! (car declaration) (cadr declaration)))
                          (let-declarations exp))))
    (cons 'let
          (cons unassigned-declarations
                (append assignments (let-body exp))))))

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) 
  (cadr proc))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (define (iter vars vals)
    (if (or (null? vars)
            (null? vals))
        '()
        (cons (list (car vars) (car vals))
              (iter (cdr vars) (cdr vals)))))
  (if (or (null? variables)
          (null? values))
      '((nil nil))
      (iter variables values)))

(define (add-binding-to-frame! var val frame)
  (let iter ((f frame))
    (if (null? (cdr f))
        (set-cdr! f (list (list var val)))
        (iter (cdr f)))))

(define (extend-environment vars vals base-env)
  (cond ((and (not (null? vars))
              (eq? (car vars) 'splat))
         (cons (make-frame (list (cadr vars))
                           (list (eval (list->lazy-list vals) base-env)))
               base-env))
        ((= (length vars) (length vals))
         (cons (make-frame vars vals) base-env))
        (else
         (if (< (length vars) (length vals))
             (error "Too many arguments supplied" 
                    vars 
                    vals)
             (error "Too few arguments supplied" 
                    vars 
                    vals)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (traverse-env var
                      env
                      (lambda (definitions var env)
                        (if (eq? (cadar definitions) "*unassigned*")
                            (error "Variable has not yet been assigned: " var)
                            (cadar definitions)))
                      (lambda (definitions var env)
                        (env-loop (enclosing-environment env))))))
  
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (traverse-env var
                      env
                      (lambda (definitions var env)
                        (set-car! (cdar definitions) val))
                      (lambda (definitions var env)
                        (env-loop (enclosing-environment env))))))
  (env-loop env))

(define (define-variable! var val env)
  (traverse-env var
                env
                (lambda (definitions var env)
                  (set-car! (cdar definitions) val))
                (lambda (definitions var env)
                  (add-binding-to-frame! var val (first-frame env)))))

(define (filter pred sequence)
  (cond ((null? sequence) '())
        ((pred (car sequence))
         (cons (car sequence) (filter pred (cdr sequence))))
        (else (filter pred (cdr sequence)))))

(define (make-unbound! symbol env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable MAKE-UNBOUND!: " symbol)
        (let ((new-frame (filter (lambda (binding)
                                   (not (eq? (car binding) symbol)))
                                 (first-frame env))))
          (if (= (length new-frame)
                 (length (first-frame env)))
              (env-loop (enclosing-environment env))
              (set-car! env new-frame)))))
  (env-loop env))

(define (traverse-env var env found-var-proc not-found-var-proc)
  (define (scan definitions)
    (cond ((null? definitions)
           (not-found-var-proc definitions var env))
          ((eq? var (caar definitions))
           (found-var-proc definitions var env))
          (else (scan (cdr definitions)))))
  (scan (first-frame env)))

(define (lazy-cons a b)
  (list 'lazy-list (lambda (m) (m a b))))
(define (lazy-car z)
  ((cadr z) (lambda (a b) a)))
(define (lazy-cdr z)
  ((cadr z) (lambda (a b) b)))

(define primitive-procedures
  (list (list 'null? null?)
        (list '+ +)
        (list 'map map)
        (list 'inc inc)
        (list '= =)
        (list '- -)
        (list '* *)
        (list 'runtime runtime)
        (list '> >)
        (list '< <)
        (list '<= <=)
        (list '>= >=)
        (list 'eq? eq?)
        (list 'cons cons)
        (list 'car car)
        (list 'cdr cdr)
        (list 'display display)
        (list 'list list)
        (list 'not not)
        (list 'even? even?)
        (list 'remainder remainder)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) 
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment 
          (primitive-procedure-names)
          (primitive-procedure-objects)
          the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'nil nil initial-env)
    initial-env))

(define the-global-environment 
  (setup-environment))

(define input-prompt  ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value
                   input
                   the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define (display-stream s)
  (stream-for-each display-line s))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))


(define (announce-output string)
  (newline) (newline) (display string) (newline))

(define lazy-list-element-print-limit 5)

(define (lazy-list->list l max-elements)
  (cond ((null? l) '())
        ((< max-elements 1)
         (list '...))
        (else
         (cons (lazy-car l)
               (lazy-list->list (lazy-cdr l) (- max-elements 1))))))

(define (user-print object)
  (cond ((compound-procedure? object)
         (display 
          (list 'compound-procedure
                (procedure-parameters object)
                (procedure-body object)
                '<procedure-env>)))
        ((lazy-list? object)
         (display (lazy-list->list object lazy-list-element-print-limit)))
        (else (display object))))

(define (lazy-list? obj)
  (tagged-list? obj 'lazy-list))

(define (scan-out-defines proc-body)
  (let ((definitions (filter definition? proc-body)))
    (if (null? definitions)
        proc-body
        (list (cons 'let
                    (cons
                     (map (lambda (expression)
                            (list (definition-variable expression)
                                  '"*unassigned*"))
                          definitions)
                     (map (lambda (expression)
                            (if (definition? expression)
                                (list 'set!
                                      (definition-variable expression)
                                      (definition-value expression))
                                expression))
                          proc-body)))))))

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (measure fn loops)
  (define (run-code fn times)
    (if (> times 0)
        (begin
          (fn)
          (run-code fn (- times 1)))))
  (let ((start (runtime)))
    (run-code fn loops)
    (- (runtime) start)))
