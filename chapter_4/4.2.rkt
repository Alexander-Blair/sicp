; 4.25

; If we tried to evaluate (factorial 5), we would recurse forever, as each time, all
; arguments would be evaluated, one of which includes another call to factorial, causing
; an infinite loop.

; It should work in a normal-order language

; 4.26

; Unless as a derived expression:
(define (unless->if exp)
  (append (list 'if (list 'not (cadr exp))) (cddr exp)))

; 4.27

(define count 0)
(define (id x) (set! count (+ count 1)) x)

(define w (id (id 10)))
count
; response - 0 X it's actually 1. The definition of w defines a thunk, where only the
; inner argument is forced - the ultimate value of w is not evaluated until it's used.
w
; 10
; w is force evaluated due to the driver-loop calling `actual-value` to be able to
; print out the value.
count
; 2
; ^ Since w was force evaluated, the `id` function is called for a second time,
; therefore incrementing count once again.

; 4.28

; If we implement a simple version of map:
(define (map fn seq)
  (if (null? seq)
      '()
      (cons (fn (car seq))
            (map fn (cdr seq)))))
; And then try to run something like (map (lambda (x) (+ x 2)) '(1 2 3)), since the
; procedure is passed as an argument, it's a thunk. If we call eval, we'll try and
; apply thunk as a procedure, instead of the actual operator of the procedure

; 4.29

; Just repeating the definitions from above:
(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define (square x) (* x x))

; When evaluator does not memoize:
(square (id 10))
; response - 100
count
; response - 2

; When evaluator does memoize:
(square (id 10))
; response - 100
count
; response - 1

; 4.30

; 1.
; In Ben's for each example, the expressions are both primitive procedures, which
; are always forced.

; 2.

; Evaluating (p1 1) with original eval-sequence gives '(1 2), and does the same with
; the changed version of eval-sequence

; Evaluating (p2 1) with original eval-sequence will actually return 1, as the set!
; operation is a thunk, and is not executed within the p function. In Cy's version,
; it is forced, so the return value ends up being '(1 2)

; 3.

; Calling force-it on a non-thunk will just return the object itself, so the
; behaviour would not be affected.

; 4.

; It's hard to make this call without seeing some realistic, practical examples. The
; example used in this exercise is very contrived, and people should in theory be able
; to achieve the desired behaviour avoiding code like this.

; It'd be more consistent to keep the original behaviour, but we would potentially have
; fewer 'surprises' if we changed to use Cy's version.

; 4.31

; Since the only places that we can specify delayed arguments is in the parameters of
; a function definition body, I've extended list-of-values, which is called from apply.
; The treatment is different for primitive and compound procedures, as a compound
; function arrives as a list like (procedure (a b) (the-body) ...), whereas a primitive
; procedure is an actual procedure object.

; For the compound procedure, we just need to apply the correct delaying procedure when
; a parameter is appended with lazy or lazy-memo:
(define (list-of-values exps procedure env)
  (define (build-primitive-list exps)
    (if (no-operands? exps)
        '()
        (cons (actual-value (first-operand exps) env)
              (build-primitive-list
                (rest-operands exps)))))
  (if (compound-procedure? procedure)
      (map
        (lambda (exp parameter)
          (if (pair? parameter)
              (cond ((null? (cdr parameter))
                     (error "Invalid parameter: " parameter))
                    ((eq? (cadr parameter) 'lazy)
                     (delay-it exp env))
                    ((eq? (cadr parameter) 'lazy-memo)
                     (delay-it-memoized exp env))
                    (else (error "Unknown parameter option: " parameter)))
              (eval exp env)))
        exps
        (cadr procedure))
      (build-primitive-list exps)))

(define (delay-it-memoized exp env)
  (list 'memoizable-thunk exp env))

; We then need to make the following amendments to force it, in order to differentiate
; between a memoizable thunk and a non-memoizable one:
(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value
           (thunk-exp obj)
           (thunk-env obj)))
        ((memoizable-thunk? obj)
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

(define (memoizable-thunk? obj) (tagged-list? obj 'memoizable-thunk))

; Made the following changes in the eval function, so that when a procedure parameter
; is defined as lazy, we ensure those parameters (which surface as variables in the
; procedure environment) are forced when used:
((variable? exp)
 (let ((value (lookup-variable-value exp env)))
   (if (or (thunk? value)
           (memoizable-thunk? value))
       (force-it value)
       value)))

; 4.34

; Added a new clause in the cond statement of eval:
        ((quoted-list? exp)
         (eval (list->lazy-list (cadr exp)) env))

; Where quoted list is:
(define (quoted-list? obj)
  (and (quoted? obj)
       (list? (cdr obj))))

; And list->lazy-list is:
(define (list->lazy-list l)
  (if (null? l)
      'nil
      (list 'cons (list 'quote (car l)) (list->lazy-list (cdr l)))))

; In order to convert it back:
(define (lazy-list->list l max-elements)
  (cond ((null? l) '())
        ((< max-elements 1)
         (list '...))
        (else
         (cons (lazy-car l)
               (lazy-list->list (lazy-cdr l) (- max-elements 1))))))

; Where the lazy versions of cons, car and cdr are as follows. These are also registered
; in the primitive functions list under 'cons, 'car and 'cdr:
(define (lazy-cons a b)
  (list 'lazy-list (lambda (m) (m a b))))
(define (lazy-car z)
  ((cadr z) (lambda (a b) a)))
(define (lazy-cdr z)
  ((cadr z) (lambda (a b) b)))

; And for lazy-list check:
(define (lazy-list? obj)
  (tagged-list? obj 'lazy-list))
