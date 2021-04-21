; 3.1

(define (make-accumulator sum)
  (lambda (change)
    (set! sum (+ sum change))
    sum))

; 3.2

(define (make-monitored fn)
  (let ((counter 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) counter)
            ((eq? arg 'reset-count) (set! counter 0))
            (else (begin (set! counter (+ counter 1))
                         (fn arg)))))))

; 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                 (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password-attempt m)
    (cond ((not (eq? password password-attempt))
           (lambda (x) "Incorrect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                       MAKE-ACCOUNT" m))))
  dispatch)

; 3.4

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                 (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops x) "Calling the cops!")
  (define (incorrect-password x) "Incorrect password")
  (let ((consecutive-password-failures 0))
    (define (dispatch password-attempt m)
      (if (eq? password password-attempt)
          (begin (set! consecutive-password-failures 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT" m))))
          (begin (set! consecutive-password-failures (+ consecutive-password-failures 1))
                 (if (> consecutive-password-failures 7)
                     call-the-cops
                     incorrect-password))))
    dispatch))

; 3.5

(define (float x) (exact->inexact x))

(define (estimate-pi x1 x2 y1 y2 trials)
  (let* ((P (within-circle? x1 x2 y1 y2))
         (circle-area (estimate-integral P x1 x2 y1 y2 trials))
         (radius (abs (/ (- x1 x2) 2))))
    (float (/ circle-area (square radius)))))

(define (within-circle? x1 x2 y1 y2)
  (let ((circle-radius (abs (/ (- x1 x2) 2)))
        (centre-x (/ (+ x1 x2) 2))
        (centre-y (/ (+ y1 y2) 2)))
    (lambda (x y)
      (<= (+ (square (- x centre-x))
             (square (- y centre-y)))
          (square circle-radius)))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (let ((rectangle-area (* (abs (- x1 x2))
                           (abs (- y1 y2)))))
    (* rectangle-area
       (monte-carlo trials (lambda () (P (random-in-range (float x1) (float x2))
                                         (random-in-range (float y1) (float y2))))))))

; 3.6

(define rand
  (let ((x random-init))
    (lambda (op)
      (cond ((eq? op 'generate) (set! x (rand-update x))
                                x)
            ((eq? op 'reset) (lambda (new) (set! x new)))
            (else (error "INVALID OPERATION RAND: " op))))))

; 3.7

; An implementation without changing the solution to 3.3. If we, for example, amend
; the make-account function to accept new passwords being added, you'd be able to log
; into the main account or the joint account with either password. It seems safer to
; only allow the main account to be accessed with the main password, and the joint
; account to be accessed with the joint password:
(define (make-joint existing-acc password joint-acc-password)
  (lambda (password-attempt m)
    (if (eq? password-attempt joint-acc-password)
        (existing-acc password m)
        (lambda (x) "Incorrect password"))))

; 3.8

; Sets value to the newly passed in value, however returns the previous value
(define f
  (let ((value 0))
    (lambda (x)
      (let ((previous-value value))
        (set! value x)
        previous-value))))

; i.e.
(f 0) ; => 0
(f 1) ; => 0
(f 2) ; => 1
; and
(f 1) ; => 0
(f 0) ; => 1
