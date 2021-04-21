; 5.2

(controller
  test-counter
    (test (op >) (reg counter) (reg n))
    (branch (label factorial-done))
    (assign product (op *) (reg counter) (reg product))
    (assign counter (op +) (reg counter) (const 1))
    (goto (label test-counter))
  factorial-done)

; 5.3

(controller
    (assign guess (const 1.0))
    (goto (label compute-diff))
  compute-diff
    (assign diff (op square) (reg guess))
    (assign diff (op -) (reg diff) (reg x))
    (assign diff (op abs) (reg diff))
    (goto (label test-guess))
  test-guess
    (test (op <) (reg diff) (const 0.001))
    (branch (label sqrt-done))
    (assign t (op /) (reg x) (reg guess))
    (assign guess (op average) (reg t) (reg guess))
    (goto (label compute-diff))
  sqrt-done)

; 5.4

; 1. Recursive
(controller
    (assign continue (label exp-done))
  exp-loop
    (test (op =) (reg n) (const 0))
    (branch (label base-case))
    (save continue)
    (assign continue (label after-exp))
    (assign n (op -) (reg n) (const 1))
    (goto (label exp-loop))
  after-exp
    (restore continue)
    (assign val (op *) (reg b) (reg val))
    (goto (reg continue))
  base-case
    (assign val (const 1))
    (goto (reg continue))
  exp-done)

; 2. Iterative

(controller
    (assign counter (reg n))
    (assign product (const 1))
  exp-loop
    (test (op =) (reg counter) (const 0))
    (branch (label exp-done))
    (assign counter (op -) (reg counter) (const 1))
    (assign product (op *) (reg product) (reg b))
    (goto (label exp-loop))
  exp-done)

; 5.5

; Hand simulation of factorial of 5

; step                  continue    |   n   |  val   |  stack
; ------------------------------------------------------------
; start of fact-loop    fact-done   |   5   |   -    | empty

; end of loop 1         after-fact  |   4   |   -    | [{ n: 5, continue: fact-done }]

; end of loop 2         after-fact  |   3   |   -    | [{ n: 4, continue: after-fact },
;                                                       { n: 5, continue: fact-done }]

; end of loop 3         after-fact  |   2   |   -    | [{ n: 3, continue: after-fact },
;                                                       { n: 4, continue: after-fact },
;                                                       { n: 5, continue: fact-done }]

; end of loop 4         after-fact  |   1   |   -    | [{ n: 2, continue: after-fact },
;                                                       { n: 3, continue: after-fact },
;                                                       { n: 4, continue: after-fact },
;                                                       { n: 5, continue: fact-done }]

; base case             after-fact  |   1   |   1    | [{ n: 2, continue: after-fact },
;                                                       { n: 3, continue: after-fact },
;                                                       { n: 4, continue: after-fact },
;                                                       { n: 5, continue: fact-done }]

; restore continue      after-fact  |   2   |   1    | [{ n: 3, continue: after-fact },
; and n                                                 { n: 4, continue: after-fact },
;                                                       { n: 5, continue: fact-done }]

; multiply n by val     after-fact  |   2   |   2    | [{ n: 3, continue: after-fact },
;                                                       { n: 4, continue: after-fact },
;                                                       { n: 5, continue: fact-done }]

; restore continue      after-fact  |   3   |   6    | [{ n: 4, continue: after-fact },
; and n & multiply                                      { n: 5, continue: fact-done }]

; restore continue      after-fact  |   4   |   24   | [{ n: 5, continue: fact-done }]
; and n & multiply

; restore continue      fact-done   |   5   |   120  | empty
; and n & multiply

; finish


; Hand simulation of recursive fibonacci of 5

; step                  | continue     |   n       |  val      | stack
; ---------------------------------------------------------------------
; before fib-loop       | fib-done     |     5     |   -       | empty 

; setup fib(n - 1)      | afterfib-n-1 |     4     | no change | [5, fib-done]

; setup fib(n - 1)      | afterfib-n-1 |     3     | no change | [4, afterfib-n-1, 5, fib-done]

; setup fib(n - 1)      | afterfib-n-1 |     2     | no change | [3, afterfib-n-1, 4, afterfib-n-1, 5, fib-done]

; setup fib(n - 1)      | afterfib-n-1 |     1     | no change | [2, afterfib-n-1, 3, afterfib-n-1, 4, afterfib-n-1, 5, fib-done]

; immediate-answer      | no change    | no change |     1     | no change

; goto afterfib-n-1,    | afterfib-n-1 |     2     | no change | [3, afterfib-n-1, 4, afterfib-n-1, 5, fib-done]
; restore n & continue  |

; setup fib(n - 2)      | afterfib-n-2 |     0     | no change | [1, afterfib-n-1, 3, afterfib-n-1, 4, afterfib-n-1, 5, fib-done]

; goto fib-loop and     | no change    | no change |     0     | no change
; immediate answer

; goto afterfib-n-2     | afterfib-n-1 |     0     |     1     | [3, afterfib-n-1, 4, afterfib-n-1, 5, fib-done]

; goto afterfib-n-1,    | afterfib-n-1 |     3     | no change | [4, afterfib-n-1, 5, fib-done]
; restore n & continue  |

; setup fib(n - 2)      | afterfib-n-2 |     1     | no change | [1, afterfib-n-1, 4, afterfib-n-1, 5, fib-done]

; goto fib-loop and     | no change    | no change |     1     | no change
; immediate answer

; goto afterfib-n-2     | afterfib-n-1 |     1     |     2     | [4, afterfib-n-1, 5, fib-done]

; goto afterfib-n-1,    | afterfib-n-1 |     4     | no change | [5, fib-done]
; restore n & continue  |

; setup fib(n - 2)      | afterfib-n-2 |     2     | no change | [2, afterfib-n-1, 5, fib-done]

; setup fib(n - 1)      | afterfib-n-1 |     1     | no change | [2, afterfib-n-2, 2, afterfib-n-1, 5, fib-done]

; goto fib-loop and     | no change    | no change |     1     | no change
; immediate answer

; goto afterfib-n-1,    | afterfib-n-2 |     2     | no change | [2, afterfib-n-1, 5, fib-done]
; restore n & continue  |

; setup fib(n - 2)      | afterfib-n-2 |     0     | no change | [1, afterfib-n-2, 2, afterfib-n-1, 5, fib-done]

; goto fib-loop and     | no change    | no change |     0     | no change
; immediate answer

; goto afterfib-n-2     | afterfib-n-2 |     0     |     1     | [2, afterfib-n-1, 5, fib-done]

; goto afterfib-n-2     | afterfib-n-1 |     1     |     3     | [5, fib-done]

; goto afterfib-n-1,    | fib-done     |     5     | no change | empty
; restore n & continue

; setup fib(n - 2)      | afterfib-n-2 |     3     | no change | [3, fib-done]

; setup fib(n - 1)      | afterfib-n-1 |     2     | no change | [3, afterfib-n-2, 3, fib-done]

; setup fib(n - 1)      | afterfib-n-1 |     1     | no change | [2, afterfib-n-1, 3, afterfib-n-2, 3, fib-done]

; goto fib-loop and     | no change    | no change |     1     | no change
; immediate answer

; goto afterfib-n-1,    | afterfib-n-1 |     2     | no change | [3, afterfib-n-2, 3, fib-done]
; restore n & continue

; setup fib(n - 2)      | afterfib-n-2 |     0     | no change | [1, afterfib-n-1, 3, afterfib-n-2, 3, fib-done]

; goto fib-loop and     | no change    | no change |     0     | no change
; immediate answer

; goto afterfib-n-2     | afterfib-n-1 |     0     |     1     | [3, afterfib-n-2, 3, fib-done]

; goto afterfib-n-1,    | afterfib-n-2 |     3     | no change | [3, fib-done]
; restore n & continue

; setup fib(n - 2)      | afterfib-n-2 |     1     | no change | [1, afterfib-n-2, 3, fib-done]

; goto fib-loop and     | no change    | no change |     1     | no change
; immediate answer

; goto afterfib-n-2     | afterfib-n-2 |     1     |     2     | [3, fib-done]

; goto afterfib-n-2     | fib-done     |     2     |     5     | empty


; 5.6

; Within the afterfib-n-1 label, we restore continue, and then save it again without using it
