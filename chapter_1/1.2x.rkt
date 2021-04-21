; 1.20
; 4 times for applicative order
; It'd keep expanding forever for normal order (i.e. would never actually execute)

; 1.21

; (smallest-divisor 199) => 199
; (smallest-divisor 1999) => 1999
; (smallest-divisor 19999) => 7



; 1.22

(define (search-for-primes starting-n)
  (define (search-iter n primes-remaining)
    (cond ((= primes-remaining 0) (display "done"))
          (else (timed-prime-test n)
                (if (prime? n)
                    (search-iter (+ n 2) (- primes-remaining 1))
                    (search-iter (+ n 2) primes-remaining)))))
  (if (even? starting-n)
    (search-iter (+ starting-n 1) 3)
    (search-iter starting-n 3)))

; Runtime on prime search:
; 1,000 - 3
; 10,000 - 7 / 8
; 100,000 - 23
; 1,000,000 - 71 / 78 / 79
; Roughly increases at a rate of root 10



; 1.23

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next test-divisor)
    (if (= test-divisor 2) 3 (+ test-divisor 2)))
  (cond ((> (square test-divisor) n) 
         n)
        ((divides? test-divisor n) 
         test-divisor)
        (else (find-divisor 
               n 
               (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

; Runtime on prime search using new smallest-divisor:
; 1,000 - ~2/3
; 10,000 - ~6
; 100,000 - ~15
; 1,000,000 - ~45
;
; Doesn't run twice as fast. There's an additional equality check in the new function



; 1.24

; log2 of 1,000 is ~10
; log2 of 1,000,000 is ~20
;
; Should see roughly double the time. This roughly plays out.
; The time taken per test is affected by the number of Fermat tests we perform,
; but that wouldn't affect the order of growth
;
; Note it won't be affected by choice of random number (the number of steps is dependent on the
; number we're testing, not the random number).

; 1.25
;
; It's a very bad idea to use the fast-expt function, as we're using the n value as the exponent.
; When we're checking primes > 1,000,000, we'll be trying to do a^1,000,000, which will not be kind
; on memory.
;
; With the alternative expmod implementation, since we're checking the remainder at each stage, that
; is guaranteed to be a number smaller than n (the number of the prime test), and the maximum possible
; value would be that number squared.

(expmod 2 26 26)

1.
(remainder
  (square (expmod 2 13 26))
  26)

2.
(remainder
  (square (remainder
            (* 2 (expmod 2 12 26))
            26))
  26)

3.
(remainder
  (square (remainder
            (* 2 (remainder
                   (square (expmod 2 6 26))
                   26))
            26))
  26)

4.
(remainder
  (square (remainder
            (* 2 (remainder
                   (square (remainder
                             (square (expmod 2 3 26))
                             26))
                   26))
            26))
  26)

5.
(remainder
  (square (remainder
            (* 2 (remainder
                   (square (remainder
                             (square (remainder
                                       (* 2 (expmod 2 2 26))
                                       26))
                             26))
                   26))
            26))
  26)

6.
(remainder
  (square (remainder
            (* 2 (remainder
                   (square (remainder
                             (square (remainder
                                       (* 2
                                          (remainder
                                              (square (remainder
                                                        (* 2 (expmod 2 0 26))
                                                        26))
                                              26))
                                       26))
                             26))
                   26))
            26))
  26)

; 1.26
;
; Say if you took an exponent which was a power of 2, each time you halved the exponent, the
; number of calls to expmod would increase exponentially (cancelling out the reduction in
; the number of steps from being able to half the exponent).

; Without squaring:
(expmod 10 16 16)

(remainder
  (* (expmod 10 8 16)
     (expmod 10 8 16))
  16)

(remainder
  (* (remainder (* (expmod 10 4 16)
                   (expmod 10 4 16))
                16)
     (remainder (* (expmod 10 4 16)
                   (expmod 10 4 16))
                16))
  16)

; With squaring:
(expmod 10 16 16)

(remainder
  (square (expmod 10 8 16))
  16)

(remainder
  (square (remainder
            (square (expmod 10 4 16))
            16))
  16)

; 1.27
; Carmichael numbers: 561, 1105, 1729, 2465, 2821, and 6601
; 561 - divides by three

(define (exhaustive-fermat-test n)
  (define (test-iter test-n)
    (cond ((= (+ test-n 1) n) #t)
          (else
           (if (= (expmod test-n n n) test-n)
               (test-iter (+ test-n 1))
               #f))))
  (test-iter 1))

; 1.28
;
; a^(n - 1) % n congruent to 1 % n
;
; Take n to be 17
; a to be < 17 -> 12
; 
; (12^16) % 17 congruent to 1 % 17 (it is)

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n)
       1))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((root (expmod base (/ exp 2) m))
                (rem (remainder (square root) m)))
           (if (and (= rem 1)
                    (not (= root 1))
                    (not (= root (- m 1))))
               0
               rem)))
        (else
         (remainder 
          (* base (expmod base (- exp 1) m))
          m))))
