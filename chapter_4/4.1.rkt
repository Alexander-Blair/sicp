; 4.1

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

; 4.2

; Part 1:

; The `application?` procedure checks if the expression is a pair - aside from
; `self-evaluating?` and `variable?` clauses, every other clause check would be for a
; pair. `application?` check is effectively the final (and least specific) clause,
; meaning that something like (define x 3) would find itself in an endless loop trying
; to evaluate.

; Part 2:

; If all procedure applications started with call, we could successfully check for
; procedure applications before others:
(define (application? exp)
  (tagged-list? exp 'call))

; 4.3

(define (install-eval-package-a)
  (define (eval-quote exp env)
    (text-of-quotation exp))
  (define (eval-define exp env)
    (eval-definition exp env))
  (define (eval-assignment exp env)
    (eval-assignment exp env))
  (define (eval-if exp env)
    (eval-if exp env))
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
  (define (eval-begin exp env)
    (eval-sequence (begin-actions exp)
                   env))
  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  (put 'eval 'quote eval-quote)
  (put 'eval 'define eval-define)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda eval-lambda)
  (put 'eval 'begin eval-begin)
  (put 'eval 'cond eval-cond))

(define (eval exp env)
  (cond ((self-evaluating? exp) 
         exp)
        ((variable? exp)
         (lookup-variable-value exp env))
        ((get 'eval (car exp))
         ((get 'eval (car exp)) exp env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values 
                  (operands exp) 
                  env)))
        (else
          (error "Unknown expression type: EVAL" exp))))

; 4.4

; The functions added to the eval package:
(define (and? exp)
  (tagged-list? exp 'and))
(define (and-predicates exp)
  (cdr exp))
(define (first-and-predicate exps)
  (car exps))
(define (rest-and-predicates exps)
  (cdr exps))
(define (no-and-predicates? predicates)
  (null? predicates))
(define (eval-and-predicates predicates env)
  (cond ((no-and-predicates? predicates) true)
        ((true? (eval (first-and-predicate predicates)))
         (eval-and (rest-and-predicates predicates) env))
        (else false)))

(define (or? exp)
  (tagged-list? exp 'or))
(define (or-predicates exp)
  (cdr exp))
(define (first-or-predicate exps)
  (car exps))
(define (rest-or-predicates exps)
  (cdr exps))
(define (no-or-predicates? predicates)
  (null? predicates))
(define (eval-or-predicates predicates env)
  (cond ((no-or-predicates? predicates) false)
        ((true? (eval (first-or-predicate predicates)))
         true)
        (else (eval-or (rest-or-predicates predicates) env))))

; 4.5

(define (cond-arrow-procedure? cond-actions)
  (eq? (car cond-actions) '=>))
(define (cond-arrow-procedure cond-actions)
  (cadr cond-actions))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false     ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp
                  (cond-actions first))
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (cond-arrow-procedure? (cond-actions first))
                         ((cond-arrow-procedure (cond-actions first)) (cond-predicate first))
                         (sequence->exp
                           (cond-actions first)))
                     (expand-clauses 
                       rest))))))

; 4.6

; Helper functions for a let expression:
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

(define (let-body exp)
  (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-parameters (let-declarations exp))
                     (let-body exp))
        (let-arguments (let-declarations exp))))

; Registering in the package:
(define (eval-let exp env)
  (eval (let->combination exp) env))
(put 'eval 'let let->combination)

; Note also, that in order to allow function definitions in a let, we need to amend
; the self-evaluating? function, so that it does not try and apply a compound or
; primitive procedure:
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((or (compound-procedure? exp)
             (primitive-procedure? exp)) true)
        (else false)))

; 4.7

(define (let*->nested-lets exp)
  (expand-let*-declarations (let-declarations exp) (let-body exp)))

(define (expand-let*-declarations declarations body)
  (if (null? declarations)
      body
      (make-let (list (car declarations))
                (expand-let*-declarations (cdr declarations) body))))

; (eval (let*->nested-lets exp) env) should be sufficient, as when we recursively
; call `eval`, it should then find the procedure to be applied for `let`.

; 4.8

; We can rework just `let-body` to onboard the amended version of the function
(define (make-definition name parameters body)
  (cons 'define
        (cons (cons name parameters)
              body)))

(define (named-let? exp)
  (variable? (cadr exp)))

(define (named-let-procedure-name exp)
  (cadr exp))

(define (let-body exp)
  (if (named-let? exp)
      (list (make-definition (named-let-procedure-name exp)
                             (let-parameters (let-declarations exp))
                             (cdddr exp))
            (cons (named-let-procedure-name exp)
                  (let-parameters (let-declarations exp))))
      (cddr exp)))

(define special-let
  '(let fib-iter ((a 0) (b 1) (count n))
     (if (= count 0)
         b
         (fib-iter (+ a b)
                   a
                   (- count 1)))))
; Calling (let->combination special-let nil) results in the following being
; received by `apply` (of course with newlines added by myself!):
(lambda (a b count)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter a b count))

; 4.9

; A possible implementation for `for`:
(define (for i update-i finished? fn)
  (if (not (finished? i))
      (begin
        (fn i)
        (for (update-i i) update-i finished? fn))))

; Example usage would be the following. Worth noting that it's not a very functional
; construct, and the only value is causing side effects (like mutating variables):
(for 0
     inc
     (lambda (i) (> i 10))
     (lambda (i)
       (display i)
       (newline)))
       
; Similarly, to implement `while`:

(define (while predicate fn)
  (if (predicate)
      (begin
        (fn)
        (while predicate fn))))

(define a 0)

(while (lambda () (< a 10))
       (lambda () (set! a (+ a 1))))

; In order to evaluate a while:
(define (while-predicate exp)
  (cadr exp))
(define (while-procedure exp)
  (caddr exp))

(define (make-named-let name declarations body)
  (list 'let name declarations body))

; It can be defined in terms of a named let. Note that we have to immediately invoke
; the predicate function for it to work, otherwise, for example, we'd be checking
; true-ness of the lambda itself, not the result of calling it

(define (while->combination expr)
  (make-named-let 'iter
                  '()
                  (make-if (list (while-predicate expr))
                           (sequence->exp (list (list (while-procedure expr))
                                                (list 'iter)))
                           'true)))

; Similarly with for, to grab each part of the expression:
(define (for-index exp)
  (cadr exp))
(define (for-update-index-proc exp)
  (caddr exp))
(define (for-finished-proc exp)
  (cadddr exp))
(define (for-execution-proc exp)
  (cadddr (cdr exp)))

; We can also used a named let here too, again being careful to remember to invoke the
; procedure to check if we're finished, and the procedure to execute:
(define (for->combination exp)
  (make-named-let
    'iter
    (list (list 'i (for-index exp)))
    (make-if (list (for-finished-proc exp) 'i)
             'true
             (sequence->exp (list (list (for-execution-proc exp) 'i)
                                  (list 'iter (list (for-update-index-proc exp) 'i)))))))

; If we take the following expression:
(define ex '(for 0 inc (lambda (i) (> i 10)) (lambda (i) (display i) (newline))))
; Calling (for->if ex) gives us:
(let iter ((i 0))
  (if ((lambda (i) (> i 10)) i)
      true
      (begin ((lambda (i)
                (display i)
                (newline)) i)
             (iter (inc i)))))

; 4.11

; Assuming that all functions retain the same interface, the following will change:
(define (make-frame variables values)
  (if (or (null? variables)
          (null? values))
      '()
      (cons (list (car variables) (car values))
            (make-frame (cdr variables) (cdr values)))))

(define (add-binding-to-frame! var val frame)
  (let iter ((f frame))
    (if (null? (cdr f))
        (set-cdr! f (list (list var val)))
        (iter (cdr f)))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop 
               (enclosing-environment env)))
            ((eq? var (caar frame))
             (cadar frame))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop 
               (enclosing-environment env)))
            ((eq? var (caar frame))
             (set-car! (cdar frame) val))
            (else (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable: SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (define (scan frame)
    (cond ((null? frame)
           (add-binding-to-frame! 
             var val (first-frame env)))
          ((eq? var (caar frame))
           (set-car! (cdar frame) val))
          (else (scan (cdr frame)))))
  (scan (first-frame env)))

; Note, we no longer need `frame-variables` or `frame-values` any more

; 4.12

; One potential way of abstracting the pattern would be to define a traverse-env
; procedure, that would provide two functions, which would decide what to do when
; the variable is found, and when the variable is not found (i.e. when we have no
; more definitions to check)

(define (traverse-env var env found-var-proc not-found-var-proc)
  (define (scan definitions)
    (cond ((null? definitions)
           (not-found-var-proc definitions var env))
          ((eq? var (caar definitions))
           (found-var-proc definitions var env))
          (else (scan (cdr definitions)))))
  (scan (first-frame env)))

; The lookup, set and define functions can then be rewritten to provide the relevant
; functions:
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (traverse-env var
                      env
                      (lambda (definitions var env)
                        (cadar definitions))
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

; 4.13

; Since child frames of a given environment share the same pointers to variables,
; we should definitely ensure that child frames see the same unbinding of symbols
; as their parents. Whether we should search recursively in parent frames is another
; question - if both a child frame and one or more parent frames had a binding for a
; given symbol, we'd have the choice to:
; - only remove the child frame's binding
; - remove all bindings recursively

; If a child frame did not have a binding for a given symbol, but at least one parent
; frame did, we could either:
; - do nothing
; - remove the first matching binding
; - remove all matching bindings

; The recursive option seems especially dangerous, as bindings to small local functions
; will sometimes have single letter variable names such as `x`, which would result in
; confusion if parent envs also contained functions defining a variable with the same
; name. So, it seems, we should either choose the option of only searching in the current
; environment, or removing the first match.

; Only traversing the current environment:
(define (make-unbound! symbol env)
  (set-car! env (filter
                 (lambda (binding)
                   (display binding)
                   (newline)
                   (not (eq? (car binding) symbol)))
                 (first-frame env))))

; Recursing up the enclosing environments until the first binding is found:
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

; 4.15

; If we evaluate (try try), we'd have to evaluate (halts? try try). When the halts?
; procedure returns true, it'll run forever, however for halts to return true, the
; try procedure would have to return a value (i.e. 'halted). This can only happen if
; halts? returns false. Equally, when the halts? procedure returns false, the try
; procedure would return 'halted, which would cause halts? to return true (since try
; returned a value).

; 4.16

; 1.
(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (traverse-env var
                      env
                      (lambda (definitions var env)
                        (if (eq? (cadar definitions) "*unassigned*")
                            (error "Variable has not yet been assigned: " (caar definitions))
                            (cadar definitions)))
                      (lambda (definitions var env)
                        (env-loop (enclosing-environment env))))))

  (env-loop env))

; 2.

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

; A body such as the example:
'((define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))

; Becomes:

'((let ((even? "*unassigned*")
        (odd? "*unassigned*"))
    (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
    (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
    (even? x)))

; Note I've used a string instead of a symbol, as when evaluating ANY symbol, it will
; assume that it's a variable and look it up in the environment. This means it would
; attempt to find a defined variable '*unassigned* and blow up. Note this fails for
; any attempt to write a let clause that defines a variable with a symbol value.

; 3. It'd be better to do this in make-procedure, as we'd only call it once per
; procedure definition, instead of each time it was called.

; 4.18

; The alternative strategy will not work, as the value of <e2> relies on the definition
; of y, however that will not be defined until AFTER the entire let block. This could
; work if we used let* (but there seems to be no real benefit from doing this).

; 4.19

; Raising error seems like the most sensible option, to prevent any unexpected results.
; Most of the time, these sorts of things should be written in a less ambiguous way.

; 4.20

; 1.
; letrec->let definition:

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

; 2. Using let, the environment associated with the odd? and even? lambdas is the
;    one enclosing the actual lambda body created by the let expression, meaning that
;    they do not see the bindings made by the let

; 4.21

; 1.
; To calculate fib of 10:
((lambda (n)
   ((lambda (fib) (fib fib 1 0 n))
    (lambda (f a b k)
      (if (= k 1)
          a
          (f f (+ a b) a (- k 1))))))
 10)

; 2.
(define (fn x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) 
         true 
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) 
         false 
         (ev? ev? od? (- n 1))))))

; 4.22

; Helper function:
(define (let? exp)
  (tagged-list? exp 'let))

; Condition added to the cond:
((let? exp)
 (analyze (let->combination exp)))

; 4.23

; The two versions will do a similar amount of work for only one expression, however
; once we have two or more, the original version will only have a single procedure to
; call, however Eva's version will have to loop through all the sub-expressions on each
; evaulation.

; 4.24

; The analysis version is much quicker for any function definitions, as the procedure gets
; stored in the env, and the next time we just need to call the procedure. Having done some
; testing, there's no clear answer on the exact time spent in analysis, but for a cond
; expression, we saw roughly 1/3 of time spent on analysis, however for a simple function
; definition, as shown below:
(define (g x)
  (let ((a 12))
    (* a x 10)))
; it seems that roughly 80% of time was spent in analysis
