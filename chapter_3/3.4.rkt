; 3.38

; 1. Possible values are 35, 40, 45, 50

; 2. Drew some diagrams but basically it is anyone's guess. The most extreme case could be
;    all commands reading the balance before any other command had called set!, and then
;    each command would ignore the result of the other. This would would effectively result
;    in only one of the commands affecting the balance.

; 3.39

; 1. Of course we can have the two 'normal' results of 101 and 121, but we can also have
;    a result of 100, if (s (lambda () (* x x))) executes, followed by the second serialized
;    procedure, and THEN (set! x value) is called where the value is the result of the initial
;    (* x x)

; 3.40

; Based on the following constraints:
;  - the set! in each lambda cannot be called before both reads have happened;
;  - the order in which the reads happen between two writes is not important,
;    since they will all read the same value of x.

; There are a few possible configurations of reads and writes:
; - 5 reads, 2 writes
; - 4 reads, 1 write, 1 read, 1 write
; - 3 reads, 1 write, 2 reads, 1 write
; - 2 reads, 1 write, 3 reads, 1 write

; If we give each operation its own number like so:
;              3        1 2
; (lambda () (set! x (* x x)))
;              7        4 5 6
; (lambda () (set! x (* x x x)))

; The possible distinct combinations would be (I've bracketed the reads, as the order in
; which these happen is not important, since the value will be read the same for each:
; - (1, 2, 4, 5, 6), 7, 3   => 100
; - (1, 2, 4, 5, 6), 3, 7   => 1,000
; - (1, 2, 4, 5), 3, (6), 7 => 10,000
; - (1, 4, 5, 6), 7, (2), 3 => 10,000
; - (1, 2, 4), 3, (5, 6), 7 => 100,000
; - (4, 5, 6), 7, (1, 2), 3 => 1,000,000
; - (1, 2), 3, (4, 5, 6), 7 => 1,000,000

; If we serialize both procedures like so
(parallel-execute 
 (s (lambda () (set! x (* x x))))
 (s (lambda () (set! x (* x x x)))))
; The only remaining possible result is 1,000,000

; 3.41

; It doesn't make much of a difference. Say if someone was concurrently trying to withdraw
; money from the account, and the order of serialization was read balance, followed by
; withdraw amount, the balance would very soon be outdated anyway. In the opposite order,
; without serialization, if the calculation of the new balance happened for withdrawal,
; followed by the read of the balance, followed by the setting of the new balance, the
; read balance would shortly be out of date. Though given these were executed in parallel,
; none of the results would be guaranteed or incorrect.

; 3.42

; The change to define the protected deposit and withdraw functions should not make a
; difference to the concurrency of the bank account.

; 3.43

; If the processes are running sequentially, this means that the balances that are read
; are always guaranteed to not be a mid-procedure representation (i.e. not about to be
; mutated while another process is reading and also planning to write.

; Made a paper diagram to illustrate this! Say if we start with:
; - a1 balance $10
; - a2 balance $20
; - a3 balance $30

; When we exchange a1 & a2, and a1 & a3, if the reads of the balances happen before either
; write, we'll end up with both a2 and a3 being changed to a1's old balance ($10), and a1
; ending up with $40. The total monetary value is preserved

; If there is no serialization whatsoever, the two withdraw procedures called on a1 account
; could overwrite each other i.e. one could calculate the new balance while the other was
; also calculating the new balance. Both would then write their new balance (in either
; order), ignoring the result of the other's change.

; 3.44

; We shouldn't need a more sophisticated method in this case, as the key difference is that
; in the exchange procedure, we're reading the balance of each account in order to calculate
; the difference, before writing the new balance. In the transfer case, the amount is
; predetermined, so given that the deposit and withdrawal operations are serialized on each
; account, the total money will be preserved.

; 3.45

; If we took one of the exchange accounts, we'd first of all call the serializer to grab the
; mutex, however we would not let go of that mutex when the deposit or withdraw operation
; was called, meaning that operation would be waiting forever for the mutex to be released.

; 3.46

; Diagram written on paper, however looks something like:
;                        TIME
; Process 1                |        Process 2
;                          |
;      is mutex free? yes--|
;                          |--is mutex free? yes
;                          |
;                          |--acquire mutex - success!
; acquire mutex - success!-|
;                         \|/

; 3.47

; Pt. 1:
(define (make-semaphore n)
  (let ((lock (make-mutex))
        (taken 0))
    (define (semaphore m)
      (cond ((eq? m 'acquire)
             (lock 'acquire)
             (if (< taken n)
                 (begin (set! taken (+ taken 1))
                        (lock 'release))
                 (begin (lock 'release)
                        (semaphore 'acquire))))
            ((eq? m 'release)
             (lock 'acquire)
             (set! taken (- taken 1))
             (lock 'release))))
    semaphore))

; Pt. 2:
(define (make-semaphore n)
  (let ((lock (list false))
        (taken 0))
    (define (semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! lock)
                 (semaphore 'acquire))
             (if (< taken n)
                 (begin (set! taken (+ taken 1))
                        (clear! lock))
                 (begin (clear! lock)
                        (semaphore 'acquire))))
            ((eq? m 'release)
             (if (test-and-set! lock)
                 (semaphore 'release))
             (set! taken (- taken 1))
             (clear! lock))))
    semaphore))

; 3.48

; Incrementing number generator (using a serializer to prevent race conditions):
(define get-incrementing-id
  (let ((current-id false)
        (protected (make-serializer)))
    (protected
     (lambda ()
       (if (not current-id)
           (set! current-id 0)
           (set! current-id (+ current-id 1)))
       current-id))))

; Account initializer now looks like:
(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin 
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer 
         (make-serializer))
        (account-number (get-account-number)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'account-number) account-number)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

; And the new definition of serialized-exchange. We order the serializers by account
; number, using the account with the lowest account number as the innermost serializer:
(define (serialized-exchange account1 account2)
  (define (serializer-wrapper fn)
    (let ((account-number-1 (account1 'account-number))
          (account-number-2 (account2 'account-number))
          (serializer1 (account1 'serializer))
          (serializer2 (account2 'serializer)))
      (cond ((eq? account-number-1 account-number-2)
             (error "SERIALIZED EXCHANGE: account1 and account2 are the same account"))
            ((< account-number-1 account-number-2)
             (serializer2 (serializer1 fn)))
            (else
             (serializer1 (serializer2 fn))))))
  ((serializer-wrapper exchange) account1 account2))

; Note that in this case it would work equally if we used the opposite order.

; 3.49

; If we wanted to exchange two accounts, but we did not know which accounts they were
; before performing the exchange, and we'd already grabbed a mutex on one of the accounts
; before loading the second account, that would suffer from the initial issue, as, by the
; time we needed to order the serializations, we'd already have a lock on one of the accounts
