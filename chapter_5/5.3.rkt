; 5.20

;       ___     ____ 
; (y)  |.|.|-> |.| /|
;      |_|_|   |_|/_|
;       ||    //
;       ||   //
;       ||  //
;       || //
;       ||//
;       _\/     ____  
; (x)  |.|.|-> |.| /|
;      |_|_|   |_|/_|
;       |       |
;       _       _
;      |1|     |2|
;      |_|     |_|

;            _________________________
;            | 0 | 1  | 2  | 3  | 4  |
; ___________|___|____|____|____|____|
; | the-cars |   | n1 | n2 | p1 | p1 |
; |----------|---|----|----|----|----|
; | the-cdrs |   | p2 | e0 | p4 | e0 |
; ------------------------------------

; x points to p1, and y points to p3

; 5.21 using car and cdr as primitives:

; Part 1 (recursive):
(define cl-machine-1
  (make-machine
    (list (list '+ +)
          (list 'car car)
          (list 'cdr cdr)
          (list 'null? null?)
          (list 'not not)
          (list 'pair? pair?))
    '(controller
        (assign continue (label count-leaves-done))
      main-loop
        (test (op null?) (reg tree))
        (branch (label null-base-case))
        ; Setup test for pair-ness
        (assign is-pair (op pair?) (reg tree))
        (test (op not) (reg is-pair))
        (branch (label not-pair-base-case))
        ; Setup count of car tree
        (save continue)
        (assign continue (label count-cdr))
        (save tree)
        ; Continue count of cdr tree
        (assign tree (op car) (reg tree))
        (goto (label main-loop))
      count-cdr
        (restore tree)
        (save val)
        (assign continue (label after-cdr))
        (assign tree (op cdr) (reg tree))
        (goto (label main-loop))
      after-cdr
        (assign tmp (reg val))
        (restore val)
        (assign val (op +) (reg tmp) (reg val))
        (restore continue)
        (goto (reg continue))
      null-base-case
        (assign val (const 0))
        (goto (reg continue))
      not-pair-base-case
        (assign val (const 1))
        (goto (reg continue))
      count-leaves-done)))

; Part 2 (with explicit counter):
(define cl-machine-2
  (make-machine
    (list (list '+ +)
          (list 'car car)
          (list 'cdr cdr)
          (list 'null? null?)
          (list 'not not)
          (list 'pair? pair?))
    '(controller
       (assign continue (label count-leaves-done))
       (assign n (const 0))
     main-loop
       (test (op null?) (reg tree))
       (branch (label null-base-case))
       (assign is-pair (op pair?) (reg tree))
       (test (op not) (reg is-pair))
       (branch (label not-pair-base-case))
       ; Setup count of cdr
       (save continue)
       (assign continue (label count-cdr))
       (save tree)
       ; Continue with count of car
       (assign tree (op car) (reg tree))
       (goto (label main-loop))
     count-cdr
       (restore tree)
       (restore continue)
       (assign tree (op cdr) (reg tree))
       (goto (label main-loop))
     null-base-case
       (goto (reg continue))
     not-pair-base-case
       (assign n (op +) (reg n) (const 1))
       (goto (reg continue))
     count-leaves-done)))

; 5.22

; append, based on the following implementation:
(define (append a b)
  (if (null? a)
      b
      (cons (car a)
            (append (cdr a) b))))

(define append-machine
  (make-machine
    (list (list 'car car)
          (list 'cdr cdr)
          (list 'cons cons)
          (list 'null? null?))
    '(controller
       (assign continue (label append-done))
     main-loop
       (test (op null?) (reg a))
       (branch (label null-base-case))
       (save continue)
       (assign continue (label combine))
       (save a)
       (assign a (op cdr) (reg a))
       (goto (label main-loop))
     combine
       (restore a)
       (restore continue)
       (assign car-a (op car) (reg a))
       (assign val (op cons) (reg car-a) (reg val))
       (goto (reg continue))
     null-base-case
       (assign val (reg b))
       (goto (reg continue))
     append-done)))

; append!, based on the following implementation:
(define (append! a b)
  (cond ((null? a) b)
        ((null? (cdr a)) (set-cdr! a b))
        (else (append! (cdr a) b))))

(define append!-machine
  (make-machine
    (list (list 'set-cdr! set-cdr!)
          (list 'cdr cdr)
          (list 'null? null?))
    '(controller
       (assign continue (label append-done))
       ; Create a separate pointer so that `a` still points to the beginning of the
       ; combined list when the machine finishes
       (assign pointer (reg a))
     main-loop
       (test (op null?) (reg pointer))
       (branch (label null-base-case-1))
       (assign the-cdr (op cdr) (reg pointer))
       (test (op null?) (reg the-cdr))
       (branch (label null-base-case-2))
       (assign pointer (op cdr) (reg pointer))
       (goto (label main-loop))
     null-base-case-1
       (assign a (reg b))
     null-base-case-2
       (perform (op set-cdr!) (reg pointer) (reg b))
     append-done)))

;;;;;

; 5.24

; Instructions for processing a cond clause; this includes checks to ensure an else
; clause is definitely at the end of the cond clause.
ev-cond
  (assign unev (op cond-clauses) (reg exp))
  (save continue)
ev-cond-clause-loop
  (assign exp (op first-exp) (reg unev))
  (test (op cond-else-clause?) (reg exp))
  (branch (label ev-cond-else))
  (assign exp (op cond-predicate) (reg exp))
  (save unev)
  (save env)
  (assign continue (label ev-cond-clause-decide))
  (goto (label eval-dispatch))
ev-cond-clause-decide
  (restore env)
  (restore unev)
  (test (op true?) (reg val))
  (branch (label ev-cond-actions))
  (test (op last-exp?) (reg unev))
  (branch (label ev-cond-no-match))
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-cond-clause-loop))
ev-cond-else
  (test (op last-exp?) (reg unev))
  (branch (label ev-cond-actions))
  (assign val (const cond-bad-else-error))
  (goto (label signal-error))
ev-cond-actions
  (assign exp (op first-exp) (reg unev))
  (assign unev (op cond-actions) (reg exp))
  (goto (label ev-sequence))
ev-cond-no-match
  (assign val (const nil))
  (goto (reg continue))

; 5.25

; The primary changes that need to be made are as follows:

; 1. Instead of evaluating arguments while building the arglist, create a thunk object,
;    which contains the expression and env for it to be evaluated against.
; 2. When a primitive procedure is about to be applied, you must then iterate through
;    the list again, evaluating the expression and env of the thunk.

; We introduce four new 'primitive' procedures:
        (list 'thunk? thunk?)
        (list 'thunk-exp thunk-exp)
        (list 'thunk-env thunk-env)
        (list 'delay-it delay-it)

; For now, the way we resolve thunks is at the point of grabbing a variable. If the value
; of the variable is a thunk, we evaluate the thunk. It may also be possible to check this
   ev-variable
     (assign val
             (op lookup-variable-value)
             (reg exp)
             (reg env))
     (test (op thunk?) (reg val))
     (branch (label ev-thunk-variable))
     (goto (reg continue))
   ev-thunk-variable
     (assign exp (reg val))
     (goto (label eval-dispatch))

; And the instructions to evaluate a thunk:
   ev-thunk
     (save env)
     (save continue)
     (assign env (op thunk-env) (reg exp))
     (assign exp (op thunk-exp) (reg exp))
     (assign continue (label ev-after-thunk))
     (goto (label eval-dispatch))
   ev-after-thunk
     (restore continue)
     (restore env)
     (goto (reg continue))

; We add the thunk? check immediately after self-evaluating check:
   eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op thunk?) (reg exp))
     (branch (label ev-thunk))
     ...

; ev-appl-operand-loop becomes significantly simpler since we don't evaluate
; any of the arguments off the bat:
   ev-appl-operand-loop
     (assign exp (op first-operand) (reg unev))
     (assign exp (op delay-it) (reg exp) (reg env))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (assign argl
             (op adjoin-arg)
             (reg exp)
             (reg argl))
     (assign unev
             (op rest-operands)
             (reg unev))
     (goto (label ev-appl-operand-loop))
   ev-appl-last-arg
     (assign argl 
             (op adjoin-arg)
             (reg exp)
             (reg argl))
     (goto (label apply-dispatch))

; We effectively reintroduce the logic that was removed from the previous subroutine
; when we eventually have to evaluate the arguments (before applying a primitive procedure)
   ev-primitive-accumulate-arg
     (test (op no-operands?) (reg unev))
     (branch (label primitive-apply))
     (perform (op user-print) (reg proc))
     (assign unev (reg argl))
     (assign argl (op empty-arglist))
     (save proc)
   ev-appl-operand-loop-2
     (save argl)
     (assign exp
             (op first-operand)
             (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg-2))
     (save env)
     (save unev)
     (assign continue 
             (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
   ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl 
             (op adjoin-arg)
             (reg val)
             (reg argl))
     (assign unev
             (op rest-operands)
             (reg unev))
     (goto (label ev-appl-operand-loop-2))
   ev-appl-last-arg-2
     (assign continue 
             (label ev-appl-accum-last-arg-2))
     (goto (label eval-dispatch))
   ev-appl-accum-last-arg-2
     (restore argl)
     (assign argl 
             (op adjoin-arg)
             (reg val)
             (reg argl))
     (perform (op user-print) (reg argl))
     (restore proc)
     (goto (label primitive-apply))

; 5.26

; 1. Maximum depth is always 10

; 2. Total number of push operations is 35n + 29. As you can see, the total number of
; pushes increases by 35 each time. The we just need a top up of 29 to make it up to
; the total push number:

; (factorial 3) - 134 pushes
; (factorial 4) - 169 pushes
; (factorial 5) - 204 pushes
; (factorial 6) - 239 pushes
; (factorial 13) - 484 pushes

;---------------------|-----------|------------------|
;                     | max-depth | number-of-pushes |
;---------------------|-----------|------------------|
; recursive-factorial |  5n + 3   |    32n - 16      |
;---------------------|-----------|------------------|
; iterative-factorial |    10     |    35n + 29      |
;---------------------|-----------|------------------|
