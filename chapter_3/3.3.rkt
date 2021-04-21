; 3.24

; We can define the constructor as so, taking a same-key? function, internally defining
; the assoc function to make use of the same-key? function:
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) 
             (car records))
            (else (assoc key (cdr records)))))
    ...))

; 3.25

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (table? t)
      (and (pair? t)
           (pair? (cdr t))
           (pair? (car (cdr t)))))
    (define (lookup keys)
      (define (recursive-lookup keys table)
        (cond ((not (table? table)) false)
              ((null? (cdr keys))
               (let ((record (assoc (car keys) (cdr table))))
                 (if record (cdr record) false)))
              (else
                (let ((subtable (assoc (car keys) (cdr table))))
                  (if subtable
                      (recursive-lookup (cdr keys) subtable)
                      false)))))
      (if (null? keys)
          (error "LOOKUP: no keys provided!"))
      (recursive-lookup keys local-table))
    (define (insert! keys value)
      (define (recursive-insert! keys table value)
        (cond ((null? (cdr keys))
               (let ((record (assoc (car keys) (cdr table))))
                 (if (table? record) (error "INSERT: Cannot overwrite table!"))
                 (if record
                     (set-cdr! record value)
                     (set-cdr! table (cons (cons (car keys) value)
                                           (cdr table))))))
              (else
                (let ((subtable (assoc (car keys) (cdr table))))
                  (if subtable
                      (recursive-insert! (cdr keys) subtable value)
                      (let ((subtable (list (car keys))))
                        (set-cdr! table (cons subtable (cdr table)))
                        (recursive-insert! (cdr keys) subtable value)))))))
      (if (null? keys)
          (error "INSERT: no keys provided!"))
      (recursive-insert! keys local-table value)
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: 
                         TABLE" m))))
                      dispatch))

; 3.27

; When we define memo-fib in the following way, it does not work as planned:
(define memo-fib
  (memoize fib))
; This is because when we call (memo-fib 10), for example, the memoize function
; will only be called once. When we investigate the function below, we can see
; that when `fib` is recursively called, it will skip the memoization wrapper.
; In the end, we are memoizing the final output of the function, instead of each
; intermediate step (which gives significantly less benefit!)
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else 
         (+ (fib (- n 1))
            (fib (- n 2))))))

; 3.28

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal a1) 
                        (get-signal a2))))
      (after-delay 
        or-gate-delay
        (lambda ()
          (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signals" s1 s2))))

; 3.29

(define (or-gate-b a1 a2 output)
  (let ((a3 (make-wire)) (a4 (make-wire)) (a5 (make-wire)))
    (inverter a1 a3)
    (inverter a2 a4)
    (and-gate a3 a4 a5)
    (inverter a5 output)
    'ok))

; In terms of or-gate delay, it'd be 3 inverter delays + 1 and delay

; 3.30

(define (ripple-carry-adder a-bits b-bits sum-bits c-in)
  (if (null? a-bits)
      sum-bits
      (let ((c-out (make-wire)))
        (full-adder (car a-bits) (car b-bits) c-in (car sum-bits) c-out)
        (ripple-carry-adder (cdr a-bits) (cdr b-bits) (cdr sum-bits) c-out))))

; The full adder total delay would equal:
; n * ((3 * or-gate-delay) + (4 * and-gate-delay) + (2 * inverter-delay))

; 3.31

; The agenda wouldn't actually be populated if we did not also execute the action

; 3.32

; If we take the example specified in the exercise:
(define a (make-wire))
(define b (make-wire))
(define out (make-wire))
(and-gate a b out)
; - Adds one action-procedure to a (and-action-procedure), which will set out to 0
; - Adds one action-procedure to b (and-action-procedure), which will set out to 0
; - Triggers both, adding two items to the agenda
(set-signal! a 0)
; Does nothing, as the value has not changed
(set-signal! b 1)
; Retriggers the action-procedures for b, which adds the one existing procedure to the
; agenda for the second time. This will also set out to 0 as only one of the wires has
; a 1 signal
(propagate!)
; If we were using FIFO execution order, we'd execute the procedures backwards, but it
; wouldn't make a difference at this point as they are all setting out to 0

(set-signal! a 1)
; Retriggers the action-procedures for a, which adds the one existing procedure to the
; agenda. This will set out to 1, since at this point a and b are both 1
(set-signal! b 0)
; Retriggers the action-procedures for b, which adds the one existing procedure to the
; agenda. This will set the output to 0
(propagate!)
; If we are using FIFO at ths point, we'll execute the agenda procedures in the reverse
; order they were added, so first setting output to 0, THEN setting it to 1, which is of
; course the incorrect expected final state of the output wire, given that a is 1 and b is 0.

; 3.33

(define (averager a b c)
  (let ((d (make-connector))
        (e (make-connector)))
    (adder a b d)
    (multiplier c e d)
    (constant 2 e)
    'ok))

; 3.34

; The problem is that the constraint only works one way - if you set the value of a, the
; value of b will be worked out, however if you do it the opposite way round and populate
; b first, since we have two unknowns, we cannot work out the value of a.

; 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0: SQUARER" (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (let ((a-value (get-value a)))
              (set-value! b
                          (* a-value a-value)
                          me)))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else (error "Unknown request: SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

; 3.37

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder y z x)
    z))

(define (cv v)
  (let ((x (make-connector)))
    (constant v x)
    x))
