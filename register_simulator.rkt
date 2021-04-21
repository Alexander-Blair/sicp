#lang sicp

(#%require (only racket/base symbol<?))

; (define fact-machine
;   (make-machine
;     (list (list '= =)
;           (list '- -)
;           (list '* *))
;     '(controller
;        (assign continue (label fact-done))   ; set up final return address
;      fact-loop
;        (test (op =) (reg n) (const 1))

;        (branch (label base-case))
;        (save continue)                       ; Set up for the recursive call
;        (save n)                              ; by saving n and continue.
;        (assign n (op -) (reg n) (const 1))   ; Set up continue so that the
;        (assign continue (label after-fact))  ; computation will continue
;        (goto (label fact-loop))              ; at after-fact when the
;      after-fact                            ; subroutine returns.
;        (restore n)
;        (restore continue)
;        (assign val (op *) (reg n) (reg val)) ; val now contains n(n - 1)!
;        (goto (reg continue))                 ; return to caller
;      base-case
;        (assign val (const 1))                ; base case: 1! = 1
;        (goto (reg continue))                 ; return to caller
;      fact-done)))

; (define (run-fact-machine)
;   (fact-machine 'trace-on)
;   (for-each (lambda (n)
;               (if (= n 3)
;                   ((fact-machine 'register-trace-on) 'n)
;                   ((fact-machine 'register-trace-off) 'n))
;               (set-register-contents! fact-machine 'n n)
;               (start fact-machine)
;               (newline)
;               (display "The n value: ")
;               (display n)
;               (newline)
;               (display "The result: ")
;               (display (get-register-contents fact-machine 'val))
;               (newline)
;               (display "Total machine instructions: ")
;               (display (fact-machine 'instructions-count))
;               (fact-machine 'reset-instructions-count)
;               (let ((stack (fact-machine 'stack)))
;                 (stack 'print-statistics)
;                 (stack 'initialize)))
;             '(3 4 5)))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
               (if trace-on
                   (begin
                     (newline)
                     (display "REGISTER NAME: ")
                     (display name)
                     (newline)
                     (display "OLD CONTENTS: ")
                     (display contents)
                     (newline)
                     (display "NEW CONTENTS: ")
                     (display value)))
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! trace-on true))
            ((eq? message 'trace-off)
             (set! trace-on false))
            (else
             (error "Unknown request: REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth 
            (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth
                  (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes 
                     '= 
                     number-pushes
                     'maximum-depth
                     '=
                     max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'contents) s)
            ((eq? message 'initialize)
             (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request: STACK"
                    message))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value)
  ((stack 'push) value))

(define (make-register-stack register-name)
  (list register-name (make-stack)))

(define (get-stack register-stack)
  (cadr register-stack))

(define (filter proc seq)
  (cond ((null? seq) '())
        ((proc (car seq))
         (cons (car seq)
               (filter proc (cdr seq))))
        (else (filter proc (cdr seq)))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instructions-list '())
        (entry-point-registers '())
        (stack-dependent-registers '())
        (register-assignments '())
        (instructions-count 0)
        (trace-on false)
        (breakpoints '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda ()
                          (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () 
                          (stack 'print-statistics)))))
          (register-table
            (list (list 'pc pc) 
                  (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
              (cons (list name (make-register name))
                    register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (begin
                (allocate-register name)
                (lookup-register name)))))
      (define (set-breakpoint! label n)
        (set! breakpoints (cons (list label n)
                                breakpoints)))
      (define (cancel-breakpoint! label n)
        (set! breakpoints (filter
                            (lambda (breakpoint)
                              (not (equal? (list label n)
                                           breakpoint)))
                            breakpoints)))
      (define (pause? instruction)
        (define (iter breaks)
          (cond ((null? breaks) false)
                ((equal? (car breaks)
                         (list (instruction 'label)
                               (instruction 'number)))
                 true)
                (else (iter (cdr breaks)))))
        (iter breakpoints))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (if trace-on
                    (begin
                      (if (not (null? (instruction-preceding-label (car insts))))
                          (begin
                            (newline)
                            (display (instruction-preceding-label (car insts)))))
                      (newline)
                      (display (instruction-text (car insts)))))
                ((instruction-execution-proc 
                   (car insts)))
                (set! instructions-count (+ instructions-count 1))
                (if (not (pause? (car insts)))
                    (execute))))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq?  message 'install-instruction-sequence)
               (lambda (seq) 
                 (set! the-instruction-sequence seq)))
              ((eq? message 'the-instruction-sequence)
               the-instruction-sequence)
              ((eq? message 'set-instructions-list)
               (lambda (instructions)
                 (set! instructions-list instructions)))
              ((eq? message 'set-entry-point-registers)
               (lambda (registers)
                 (set! entry-point-registers registers)))
              ((eq? message 'set-stack-dependent-registers)
               (lambda (registers)
                 (set! stack-dependent-registers registers)))
              ((eq? message 'set-register-assignments)
               (lambda (assignments)
                 (set! register-assignments assignments)))
              ((eq? message 'instructions-list)
               instructions-list)
              ((eq? message 'entry-point-registers)
               entry-point-registers)
              ((eq? message 'stack-dependent-registers)
               stack-dependent-registers)
              ((eq? message 'register-assignments)
               register-assignments)
              ((eq? message 'get-register) 
               lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) 
                 (set! the-ops 
                   (append the-ops ops))))
              ((eq? message 'stack)
               stack)
              ((eq? message 'initialize-stack)
               (stack 'initialize))
              ((eq? message 'print-stack-statistics)
               (stack 'print-statistics))
              ((eq? message 'operations) 
               the-ops)
              ((eq? message 'instructions-count)
               instructions-count)
              ((eq? message 'reset-instructions-count)
               (set! instructions-count 0))
              ((eq? message 'trace-on)
               (set! trace-on true))
              ((eq? message 'trace-off)
               (set! trace-on false))
              ((eq? message 'register-trace-on)
               (lambda (name)
                 ((lookup-register name) 'trace-on)))
              ((eq? message 'register-trace-off)
               (lambda (name)
                 ((lookup-register name) 'trace-off)))
              ((eq? message 'set-breakpoint)
               set-breakpoint!)
              ((eq? message 'cancel-breakpoint)
               cancel-breakpoint!)
              ((eq? message 'breakpoints)
               breakpoints)
              ((eq? message 'proceed-machine)
               (execute))
              (else (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents 
          machine register-name)
  (get-contents 
    (get-register machine register-name)))

(define (set-register-contents! 
          machine register-name value)
  (set-contents! 
    (get-register machine register-name) 
    value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    ((machine 'set-instructions-list) (sort-and-dedup-instructions insts))
                    ((machine 'set-entry-point-registers) (get-entry-point-registers insts))
                    ((machine 'set-stack-dependent-registers) (get-stack-dependent-registers insts))
                    ((machine 'set-register-assignments) (get-register-assignments insts))
                    (update-insts! insts labels machine)
                    (add-labels-and-numbers insts)
                    insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels 
        (cdr text)
        (lambda (insts labels)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (if (assoc next-inst labels)
                    (error "Duplicate label detected!: " next-inst)
                    (begin
                      (if (not (null? insts))
                          (set-preceding-label (car insts) next-inst))
                      (receive insts
                               (cons (make-label-entry next-inst insts)
                                     labels))))
                (receive (cons (make-instruction next-inst)
                               insts)
                         labels)))))))

(define (add-labels-and-numbers instructions)
  (define (iter instructions current-label n)
    (if (null? instructions)
        'done
        (let* ((inst (car instructions))
               (preceding-label (instruction-preceding-label inst)))
          (if (null? preceding-label)
              (begin
                (instruction-set-label! inst current-label)
                (instruction-set-number! inst n)
                (iter (cdr instructions) current-label (+ n 1)))
              (begin
                (instruction-set-label! inst preceding-label)
                (instruction-set-number! inst 1)
                (iter (cdr instructions) preceding-label 2))))))
  (iter instructions nil 0))

(define (select select-fn seq)
  (cond ((null? seq) '())
        ((select-fn (car seq))
         (cons (car seq)
               (select select-fn (cdr seq))))
        (else (select select-fn (cdr seq)))))

(define (group-by key-fn val-fn seq)
  (define (iter grouped rest-seq)
    (if (null? rest-seq)
        grouped
        (let* ((key (key-fn (car rest-seq)))
               (val (val-fn (car rest-seq)))
               (group (assoc key grouped)))
          (if group
              (begin
                (set-cdr! group (cons val (cdr group)))
                (iter grouped (cdr rest-seq)))
              (iter (cons (list key val) grouped)
                    (cdr rest-seq))))))
  (iter '() seq))

(define (get-entry-point-registers instructions)
  (define (register-entry-point? instruction)
    (and (goto-exp? instruction)
         (register-exp? (goto-dest instruction))))
  (let ((registers (map (lambda (inst)
                          (register-exp-reg (goto-dest inst)))
                        (select register-entry-point? (map instruction-text instructions)))))
    (dedup-sorted (sort symbol<? registers))))

(define (get-stack-dependent-registers instructions)
  (let ((registers (map stack-inst-reg-name
                        (select (lambda (inst)
                                  (or (restore-exp? inst)
                                      (save-exp? inst)))
                                (map instruction-text instructions)))))
    (dedup-sorted (sort symbol<? registers))))

(define (get-register-assignments instructions)
  (group-by assign-reg-name
            assign-value-exp
            (dedup-sorted (sort (lambda (a b)
                                  (symbol<? (car a) (car b)))
                                (select assignment-exp? (map instruction-text instructions))))))

(define (dedup-sorted instructions)
  (cond ((null? instructions) '())
        ((null? (cdr instructions)) instructions)
        ((equal? (car instructions) (cadr instructions))
         (dedup-sorted (cdr instructions)))
        (else (cons (car instructions)
                    (dedup-sorted (cdr instructions))))))

(define (sort-and-dedup-instructions instructions)
  (dedup-sorted (sort (lambda (a b)
                        (symbol<? (car a) (car b)))
                      (map instruction-text instructions))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc! inst
                                         (make-execution-procedure (instruction-text inst) 
                                                                   labels
                                                                   machine
                                                                   pc
                                                                   flag
                                                                   ops)))
      insts)))

(define (make-instruction text)
  (let ((preceding-label '())
        (label '())
        (number '())
        (proc '()))
    (define (set-proc! p)
      (set! proc p))
    (define (set-preceding-label! label)
      (set! preceding-label label))
    (define (set-label! l)
      (set! label l))
    (define (set-number! n)
      (set! number n))
    (lambda (m)
      (cond ((eq? m 'set-proc) set-proc!)
            ((eq? m 'set-preceding-label) set-preceding-label!)
            ((eq? m 'proc) proc)
            ((eq? m 'preceding-label) preceding-label)
            ((eq? m 'set-label) set-label!)
            ((eq? m 'set-number) set-number!)
            ((eq? m 'label) label)
            ((eq? m 'number) number)
            ((eq? m 'text) text)))))

(define (instruction-text inst) (inst 'text))
(define (instruction-execution-proc inst)
  (inst 'proc))
(define (instruction-preceding-label inst)
  (inst 'preceding-label))
(define (set-instruction-execution-proc! inst proc)
  ((inst 'set-proc) proc))
(define (set-preceding-label inst label)
  ((inst 'set-preceding-label) label))
(define (instruction-set-label! inst label)
  ((inst 'set-label) label))
(define (instruction-set-number! inst number)
  ((inst 'set-number) number))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label: ASSEMBLE" label-name))))

(define (make-execution-procedure 
         inst labels machine pc flag ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign 
          inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test 
          inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch 
          inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine pc))
        ((eq? (car inst) 'perform)
         (make-perform
          inst machine labels ops pc))
        (else (error "Unknown instruction type: ASSEMBLE" inst))))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp value-exp machine labels operations)
               (make-primitive-exp (car value-exp) machine labels))))
      (lambda ()   ; execution procedure
                   ; for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
                (make-operation-exp condition machine labels operations)))
          (lambda () 
            (set-contents!  flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () 
               (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (lambda ()
               (set-contents!  pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (stack (machine 'stack)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (set-contents! reg (pop (machine 'stack)))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type: ASSEMBLE" exp))))

(define (perform-action inst) (cdr inst))

(define (register-exp? exp)
  (tagged-list? exp 'reg))
(define (register-exp-reg exp)
  (cadr exp))
(define (constant-exp? exp)
  (tagged-list? exp 'const))
(define (constant-exp-value exp)
  (cadr exp))
(define (label-exp? exp)
  (tagged-list? exp 'label))
(define (label-exp-label exp) 
  (cadr exp))
(define (goto-exp? exp)
  (tagged-list? exp 'goto))
(define (restore-exp? exp)
  (tagged-list? exp 'restore))
(define (save-exp? exp)
  (tagged-list? exp 'save))
(define (assignment-exp? exp)
  (tagged-list? exp 'assign))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (or (constant-exp? e)
                        (register-exp? e))
                    (make-primitive-exp e machine labels)
                    (error "EXPECTED CONSTANT OR REGISTER EXPRESSION, GOT: " e)))
              (operation-exp-operands exp))))
    (lambda () (apply op (map (lambda (p) (p))
                              aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp)
       (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation: ASSEMBLE"
               symbol))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-a-copy seq)
  (if (null? seq)
      '()
      (cons (car seq) (make-a-copy (cdr seq)))))

(define (sort compare-fn seq)
  (define (iter seq)
    (if (<= (length seq) 1)
        seq
        (let ((split (split-in-half seq)))
          (mergesort
            compare-fn
            (iter (car split))
            (iter (cdr split))))))
  (iter (make-a-copy seq)))

(define (mergesort compare-fn seq1 seq2)
  (cond ((null? seq1) seq2)
        ((null? seq2) seq1)
        ((compare-fn (car seq1) (car seq2))
         (cons (car seq1)
               (mergesort compare-fn (cdr seq1) seq2)))
        (else (cons (car seq2)
                    (mergesort compare-fn seq1 (cdr seq2))))))

(define (split-in-half seq)
  (let ((midpoint (- (/ (length seq) 2)
                     1)))
    (define (iter rest-seq n)
      (if (>= n midpoint)
          (let ((second-half (cdr rest-seq)))
            (set-cdr! rest-seq '())
            (cons seq second-half))
          (iter (cdr rest-seq) (+ n 1))))
    (iter seq 0)))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (proceed-machine machine)
  (machine 'proceed))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        ((or (compound-procedure? exp)
             (primitive-procedure? exp)) true)
        (else false)))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (if? exp) (tagged-list? exp 'if))

(define (cond? exp) 
  (tagged-list? exp 'cond))

(define (lambda? exp) 
  (tagged-list? exp 'lambda))

(define (let? exp) 
  (tagged-list? exp 'let))

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

(define (begin? exp) 
  (tagged-list? exp 'begin))

(define (application? exp) (pair? exp))

(define the-empty-environment '())

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

(define (traverse-env var env found-var-proc not-found-var-proc)
  (define (scan definitions)
    (cond ((null? definitions)
           (not-found-var-proc definitions var env))
          ((eq? var (caar definitions))
           (found-var-proc definitions var env))
          (else (scan (cdr definitions)))))
  (scan (first-frame env)))

(define (text-of-quotation exp)
  (cadr exp))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp)
  (cddr exp))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (operands exp) (cdr exp))

(define (operator exp) (car exp))

(define (empty-arglist) '())

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (last-operand? ops)
  (and (not (null? ops))
       (null? (cdr ops))))

(define (last-exp? ops)
  (and (not (null? ops))
       (null? (cdr ops))))

(define (adjoin-arg arg args)
  (if (null? args)
      (list arg)
      (cons (car args)
            (adjoin-arg arg (cdr args)))))

; Normal order evaluation functions:
(define (actual-value exp env)
    (force-it (eval exp env)))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

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

;--------------------------

(define (apply-in-underlying-scheme proc args)
  (apply proc args))

(define (primitive-implementation proc) 
  (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (begin-actions exp) (cdr exp))

(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp)) 
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (true? x)
  (not (eq? x false)))

(define (assignment-variable exp) 
  (cadr exp))

(define (assignment-value exp) (caddr exp))

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

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))

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

(define (add-binding-to-frame! var val frame)
  (let iter ((f frame))
    (if (null? (cdr f))
        (set-cdr! f (list (list var val)))
        (iter (cdr f)))))

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

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" 
                 vars 
                 vals)
          (error "Too few arguments supplied" 
                 vars 
                 vals))))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (define-variable! var val env)
  (traverse-env var
                env
                (lambda (definitions var env)
                  (set-car! (cdar definitions) val))
                (lambda (definitions var env)
                  (add-binding-to-frame! var val (first-frame env)))))

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
                (error "ELSE clause isn't last: COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (if (cond-arrow-procedure? (cond-actions first))
                         ((cond-arrow-procedure (cond-actions first)) (cond-predicate first))
                         (sequence->exp
                          (cond-actions first)))
                     (expand-clauses 
                      rest))))))

(define (make-if predicate 
                 consequent 
                 alternative)
  (list 'if predicate consequent alternative))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

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

(define primitive-procedures
  (list (list 'null? null?)
        (list '+ +)
        (list 'map map)
        (list 'inc inc)
        (list '= =)
        (list '- -)
        (list '* *)
        (list '/ /)
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

(define (prompt-for-input string)
  (newline) (newline) 
  (display string) (newline))

(define the-global-environment 
  (setup-environment))

(define (get-global-environment)
  the-global-environment)

(define (announce-output string)
  (newline) (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display 
        (list 'compound-procedure
              (procedure-parameters object)
              (procedure-body object)
              '<procedure-env>))
      (display object)))

(define eval-controller-insts
  '(read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input)
              (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
   print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output)
              (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
   unknown-expression-type
     (assign 
       val
       (const unknown-expression-type-error))
     (goto (label signal-error))
   unknown-procedure-type
     ; clean up stack (from apply-dispatch):
     (restore continue)    
     (assign 
       val
       (const unknown-procedure-type-error))
     (goto (label signal-error))
   signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
   eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op thunk?) (reg exp))
     (branch (label ev-thunk))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))
   ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
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
   ev-quoted
     (assign val
             (op text-of-quotation)
             (reg exp))
     (goto (reg continue))
   ev-lambda
     (assign unev
             (op lambda-parameters)
             (reg exp))
     (assign exp 
             (op lambda-body)
             (reg exp))
     (assign val 
             (op make-procedure)
             (reg unev)
             (reg exp)
             (reg env))
     (goto (reg continue))
   ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch))
   ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign
       continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
   ev-appl-did-operator
     (restore unev)             ; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val))    ; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
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
   apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label ev-primitive-accumulate-arg))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))
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
   primitive-apply
     (assign val
             (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))
   compound-apply
     (assign unev 
             (op procedure-parameters)
             (reg proc))
     (assign env
             (op procedure-environment)
             (reg proc))
     (assign env
             (op extend-environment)
             (reg unev)
             (reg argl)
             (reg env))
     (assign unev
             (op procedure-body)
             (reg proc))
     (goto (label ev-sequence))
   ev-begin
     (assign unev
             (op begin-actions)
             (reg exp))
     (save continue)
     (goto (label ev-sequence))
   ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue
             (label ev-sequence-continue))
     (goto (label eval-dispatch))
   ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev
             (op rest-exps)
             (reg unev))
     (goto (label ev-sequence))
   ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))
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
     (restore continue)
     (goto (reg continue))
   ev-if
     (save exp)   ; save expression for later
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     ; evaluate the predicate:
     (goto (label eval-dispatch))
   ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
   ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
   ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))
   ev-assignment
     (assign unev 
             (op assignment-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp
             (op assignment-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue
             (label ev-assignment-1))
     ; evaluate the assignment value:
     (goto (label eval-dispatch))  
   ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val
             (const ok))
     (goto (reg continue))
   ev-definition
     (assign unev 
             (op definition-variable)
             (reg exp))
     (save unev)   ; save variable for later
     (assign exp 
             (op definition-value)
             (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     ; evaluate the definition value:
     (goto (label eval-dispatch))  
   ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!)
              (reg unev)
              (reg val)
              (reg env))
     (assign val (const ok))
     (goto (reg continue))))

(define (initialize-stack)
  (eceval 'initialize-stack))

(define (print-stack-statistics)
  (eceval 'print-stack-statistics))

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'prompt-for-input prompt-for-input)
        (list 'define-variable! define-variable!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'assignment-value assignment-value)
        (list 'assignment-variable assignment-variable)
        (list 'true? true?)
        (list 'if-consequent if-consequent)
        (list 'if-alternative if-alternative)
        (list 'if-predicate if-predicate)
        (list 'rest-exps rest-exps)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'begin-actions begin-actions)
        (list 'procedure-body procedure-body)
        (list 'extend-environment extend-environment)
        (list 'procedure-environment procedure-environment)
        (list 'procedure-parameters procedure-parameters)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'compound-procedure? compound-procedure?)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'adjoin-arg adjoin-arg)
        (list 'thunk? thunk?)
        (list 'thunk-exp thunk-exp)
        (list 'thunk-env thunk-env)
        (list 'delay-it delay-it)
        (list 'rest-operands rest-operands)
        (list 'last-operand? last-operand?)
        (list 'first-operand first-operand)
        (list 'no-operands? no-operands?)
        (list 'empty-arglist empty-arglist)
        (list 'operator operator)
        (list 'operands operands)
        (list 'make-procedure make-procedure)
        (list 'lambda-body lambda-body)
        (list 'lambda-parameters lambda-parameters)
        (list 'text-of-quotation text-of-quotation)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'application? application?)
        (list 'begin? begin?)
        (list 'lambda? lambda?)
        (list 'if? if?)
        (list 'cond? cond?)
        (list 'definition? definition?)
        (list 'assignment? assignment?)
        (list 'quoted? quoted?)
        (list 'variable? variable?)
        (list 'let? let?)
        (list 'self-evaluating? self-evaluating?)
        (list 'user-print user-print)
        (list 'announce-output announce-output)
        (list 'get-global-environment get-global-environment)
        (list 'read read)
        (list 'prompt-for-input prompt-for-input)
        (list 'initialize-stack initialize-stack)
        (list 'cond-predicate cond-predicate)
        (list 'cond-actions cond-actions)
        (list 'cond-else-clause? cond-else-clause?)
        (list 'cond-clauses cond-clauses)
        (list 'let->combination let->combination)
        (list 'print-stack-statistics print-stack-statistics)
        ))

(define eceval
  (make-machine
   eceval-operations
   eval-controller-insts))

(define (append-with-if a b)
  (if (null? a)
      b
      (cons (car a) (append-with-if (cdr a) b))))

(define (append-with-cond a b)
  (cond ((null? a) b)
        (else (cons (car a) (append-with-cond (cdr a) b)))))

; (define (try a b) (if (= a 0) 1 b))
; (try 0 (/ 1 0))
