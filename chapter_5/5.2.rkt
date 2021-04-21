; 5.8

; The first definition of `here` ends up being the one that gets registered

; In order to fix the issue, we can check for presence of the label before
; adding it to the list
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
                    (receive insts
                             (cons (make-label-entry next-inst insts)
                                   labels)))
                (receive (cons (make-instruction next-inst)
                               insts)
                         labels)))))))

; 5.9

; Amend make-operation-exp to ensure each operand is either a register or constant:
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

; 5.11

; 1. At the top of afterfib-n-2, we can replace the top two instructions with (restore n),
;    instead of firstly moving val into n, then restoring val.

; 2. 

; Create some selectors for a 'stack entry', which consists of the register name and the
; register contents
(define (make-stack-entry register-name register)
  (cons register-name (get-contents register)))

(define (stack-entry-register-name stack-entry)
  (car stack-entry))

(define (stack-entry-contents stack-entry)
  (cdr stack-entry))

; This can then be used when we save to the stack:
(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (push stack (make-stack-entry reg reg-name))
      (advance-pc pc))))

; And when we restore the stack entry, verify that the register name that saved the contents
; is the one trying to restore it:
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (let ((stack-entry (pop stack)))
        (if (eq? (stack-entry-register-name stack-entry)
                 (stack-inst-reg-name inst))
            (set-contents! reg (stack-entry-contents stack-entry))
            (begin
              (push stack stack-entry)
              (error "WRONG REGISTER TRIED TO RESTORE STACK. REGISTER THAT TRIED TO RESTORE: "
                     (stack-inst-reg-name inst)
                     "; ACTUAL IN STACK: "
                     (stack-entry-register-name stack-entry))))
        (advance-pc pc)))))

; 3.

; constructor and then a getter function for a stack associated with a particular register:
(define (make-register-stack register-name)
  (list register-name (make-stack)))

(define (get-register-stack stacks register-name)
  (let ((name-and-stack (assoc register-name stacks)))
    (if name-and-stack
        (cadr name-and-stack)
        (error "STACK NOT FOUND FOR REGISTER: " (car name-and-stack)))))

; make-save then receives all the stacks as argument, and must select the stack associated
; with the correct register:
(define (make-save inst machine stacks pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name))
         (stack (get-register-stack stacks reg-name)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

; make-restore also pops from the stack of the register:
(define (make-restore inst machine stacks pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register machine reg-name)))
    (lambda ()
      (set-contents! reg (pop (get-register-stack stacks reg-name)))
      (advance-pc pc))))

; For the make-new-machine procedure, we pass the register-names as an argument, and proceed
; to generate all the stacks, and initialize all of them as the first instruction:
(define (make-new-machine register-names)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stacks (map (lambda (reg-name)
                       (make-register-stack reg-name))
                     register-names))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list 
              (list 'initialize-stacks
                    (lambda () 
                      (for-each (lambda (stack) (stack 'initialize))
                                stacks)))))


; In addition, we change the stacks procedure to return all the stacks:
((eq? message 'stacks) stacks)

; There are several other places where we end up passing a collection of stacks, instead of a
; single one, for example in make-execution-procedure (then by extension, we pass the collection
; to make-save and make-restore

; 5.12

; First of all, we need a sort mechanism (the Racket implementation does not work on SICP lists):
(define (make-a-copy seq)
  (if (null? seq)
      '()
      (cons (car seq) (make-a-copy (cdr seq)))))

(define (sort compare-fn seq)
  (define (iter seq)
    (if (= (length seq) 1)
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

; By doing the sorting first, the deduplication step becomes simple, as we just need to
; compare adjacent elements:
(define (dedup-sorted instructions)
  (cond ((null? instructions) '())
        ((null? (cdr instructions)) instructions)
        ((equal? (car instructions) (cadr instructions))
         (dedup-sorted (cdr instructions)))
        (else (cons (car instructions)
                    (dedup-sorted (cdr instructions))))))

; For part 1:
(define (sort-and-dedup-instructions instructions)
  (dedup-sorted (sort (lambda (a b)
                        (symbol<? (car (instruction-text a)) (car (instruction-text b))))
                      instructions)))

; For part 2:

; The function to extract the entry point registers:
(define (get-entry-point-registers instructions-and-procs)
  (define (register-entry-point? instruction)
    (and (goto-exp? instruction)
         (register-exp? (goto-dest instruction))))
  (let ((registers (map (lambda (inst)
                          (register-exp-reg (goto-dest inst)))
                        (select register-entry-point? (map car instructions-and-procs)))))
    (dedup-sorted (sort symbol<? registers))))

; For part 3:

; The function to extract all registers that are saved or restored:

(define (get-stack-dependent-registers instructions)
  (let ((registers (map stack-inst-reg-name
                        (select (lambda (inst)
                                  (or (restore-exp? inst)
                                      (save-exp? inst)))
                                (map instruction-text instructions)))))
    (dedup-sorted (sort symbol<? registers))))

; For part 4:

; In order to group a given sequence, given a key and a value function. For example,
; taking '((n 1) (n 2) (m 3) (m 4)) and calling (group-by car cdr seq) would give
; you '((n (1 2)) (m (3 4))), where you can access m or n with (assoc 'n result)
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

(define (get-register-assignments instructions)
  (group-by assign-reg-name
            assign-value-exp
            (dedup-sorted (sort (lambda (a b)
                                  (symbol<? (car a) (car b)))
                                (select assignment-exp? (map instruction-text instructions))))))

; Adding all this to the assemble function:
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    ((machine 'set-instructions-list) (sort-and-dedup-instructions insts))
                    ((machine 'set-entry-point-registers) (get-entry-point-registers insts))
                    ((machine 'set-stack-dependent-registers) (get-stack-dependent-registers insts))
                    ((machine 'set-register-assignments) (get-register-assignments insts))
                    (update-insts! insts labels machine)
                    insts)))

; We've also added extra messages to the make-new-machine function to be able to set and access
; these data structures

; 5.13

; get-register has been amended to dynamically allocate the register if needed:
((eq? message 'get-register) 
 lookup-register)

(define (lookup-register name)
  (let ((val (assoc name register-table)))
    (if val
        (cadr val)
        (begin
          (allocate-register name)
          (lookup-register name)))))

; In addition, since the previous updates to allow a stack per register also depended
; on the initial list of register names, these are also dynamically allocated too:
((eq? message 'lookup-stack)
 lookup-stack)

(define (allocate-stack register-name)
  (if (assoc register-name register-stacks)
      (error "Multiply defined stack: " register-name)
      (set! register-stacks (cons (make-register-stack register-name)
                                  register-stacks)))
  'stack-allocated)

(define (lookup-stack register-name)
  (let ((register-stack (assoc register-name register-stacks)))
    (if register-stack
        (get-stack register-stack)
        (begin
          (allocate-stack register-name)
          (lookup-stack register-name)))))
              ((eq? message 'lookup-stack) lookup-stack)


; 5.14

; Note I've truncated the controller definition, but this is how the fact-machine itself is setup:
(define fact-machine
  (make-machine
    (list (list '= =)
          (list '- -)
          (list '* *))
    '(controller
       (assign continue (label fact-done))
       ...)))
 
; Allowing the machine to be run multiple times, printing out the stats and reinitializing the
; stack after each run:
(define (run-fact-machine)
  (for-each (lambda (n)
              (set-register-contents! fact-machine 'n n)
              (start fact-machine)
              (newline)
              (display "The n value: ")
              (display n)
              (newline)
              (display "The result: ")
              (display (get-register-contents fact-machine 'val))
              (let ((stack (fact-machine 'stack)))
                (stack 'print-statistics)
                (stack 'initialize)))
            '(3 4 5 6 8 10 13 17)))

; The output on the screen:

; The n value: 3
; The result: 6
; (total-pushes = 4 maximum-depth = 4)
; The n value: 4
; The result: 24
; (total-pushes = 6 maximum-depth = 6)
; The n value: 5
; The result: 120
; (total-pushes = 8 maximum-depth = 8)
; The n value: 6
; The result: 720
; (total-pushes = 10 maximum-depth = 10)
; The n value: 8
; The result: 40320
; (total-pushes = 14 maximum-depth = 14)
; The n value: 10
; The result: 3628800
; (total-pushes = 18 maximum-depth = 18)
; The n value: 13
; The result: 6227020800
; (total-pushes = 24 maximum-depth = 24)
; The n value: 17
; The result: 355687428096000
; (total-pushes = 32 maximum-depth = 32)

; To calculate the total pushes and max depth, the formula is simply 2(n - 1)

; 5.15

; instructions-count is initialized as 0:
(let ((pc (make-register 'pc))
      ...
      (instructions-count 0))
  ...)

; Adding two new messages to interface:
((eq? message 'instructions-count)
 instructions-count)
((eq? message 'reset-instructions-count)
 (set! instructions-count 0))

; Updated `execute` procedure to increment the count after each instruction:
(define (execute)
  (let ((insts (get-contents pc)))
    (if (null? insts)
        'done
        (begin
          ((instruction-execution-proc 
             (car insts)))
          (set! instructions-count (+ instructions-count 1))
          (execute)))))

; 5.16
; trace-on is initialized as false:
(let ((pc (make-register 'pc))
      ...
      (trace-on false))
  ...)

; It can be switched on and off:
((eq? message 'trace-on)
 (set! trace-on true))
((eq? message 'trace-off)
 (set! trace-on false))

; `execute` procedure is further extended to optionally print the instructions:
(define (execute)
  (let ((insts (get-contents pc)))
    (if (null? insts)
        'done
        (begin
          (if trace-on
              (begin
                (newline)
                (display (instruction-text (car insts)))))
          ((instruction-execution-proc 
             (car insts)))
          (set! instructions-count (+ instructions-count 1))
          (execute)))))

; 5.17

; We end up reworking the make-instruction procedure, so that we can
; set a preceding label on a given instruction:
(define (make-instruction text)
  (let ((preceding-label '())
        (proc '()))
    (define (set-proc! p)
      (set! proc p))
    (define (set-preceding-label! label)
      (set! preceding-label label))
    (lambda (m)
      (cond ((eq? m 'set-proc) set-proc!)
            ((eq? m 'set-preceding-label) set-preceding-label!)
            ((eq? m 'proc) proc)
            ((eq? m 'preceding-label) preceding-label)
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

; The extract-labels procedure is updated to set the preceding label on the next instruction
; in the sequence when a label is encountered:
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

; We then display the label, if present, from the execute procedure:
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
          (execute)))))

; 5.18

; Register updated to have tracing:
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

; Messages added to the make-new-machine procedure, in order to allow a specific register
; to have tracing switched on and off:
((eq? message 'register-trace-on)
 (lambda (name)
   ((lookup-register name) 'trace-on)))
((eq? message 'register-trace-off)
 (lambda (name)
   ((lookup-register name) 'trace-off)))

; 5.19

; As part of the assemble procedure, we will now attach a label and number to every
; instruction, so that we can use them to attach breakpoints:
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

; Where make-instruction has been updated to:
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

; And the new setters:
(define (instruction-set-label! inst label)
  ((inst 'set-label) label))
(define (instruction-set-number! inst number)
  ((inst 'set-number) number))

; The following call is added within the assemble procedure:
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    ...
                    (update-insts! insts labels machine)
                    (add-labels-and-numbers insts) ; here!
                    insts)))


; public procedures:
(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))

(define (proceed-machine machine)
  (machine 'proceed-machine))

; Helper procedures within make-new-machine:
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

; execute procedure is updated to pause when a breakpoint is found based on the current instruction:
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

; New messages to be received by a machine:
((eq? message 'set-breakpoint)
 set-breakpoint!)
((eq? message 'cancel-breakpoint)
 cancel-breakpoint!)
((eq? message 'breakpoints)
 breakpoints)
((eq? message 'proceed-machine)
 (execute))
