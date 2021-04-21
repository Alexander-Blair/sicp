; 1.29
; Multiplier:
; - when k equals 0 or n, 1
; - when k even, 2
; - when k odd, 4

(define (integral f a b n)
  (define h (/ (- b a) n))
  (define (inc x) (+ x 1))
  (define (y k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((= k 0) 1)
             ((= k n) 1)
             ((even? k) 2)
             (else 4))
       (y k)))
  (/ (* h (sum term 0 inc n))
     3))

; 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

; 1.31

; recursive
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

;iterative
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (approximate-pi n)
  (define (term x)
    (if (even? x)
        (/ (+ x 2) (+ x 3))
        (/ (+ x 3) (+ x 2))))
  (* 4 (product term 0 inc n)))

; 1.32

; iterative
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

; recursive
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (* (term a)
         (accumulate combiner null-value term (next a) next b))))

; 1.33

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (cond ((> a b) result)
          (else (if (filter a)
                    (iter (next a) (combiner (term a) result))
                    (iter (next a) result)))))
  (iter a null-value))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (product-of-relative-primes n)
  (define (relative-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) relative-prime?))

; gcd defined as follows:

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; 1.34

(define (f g) (g 2))

; If trying to call (f f), it would expand to (f 2), and then would attempt to
; use 2 as the function g

; 1.35

(define (golden-ratio)
  (fixed-point
   (lambda (x) (+ 1 (/ 1 x)))
   2.0))

; 1.36

define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-to-the-x x)
  (fixed-point
   (lambda (y) (/ (log x) (log y)))
   2.0))

(define (x-to-the-x-with-damping x)
  (fixed-point
   (lambda (y) (average y (/ (log x) (log y))))
   2.0))

; with damping, more than three times fewer steps

; 1.37
(define (cont-frac-recursive n d k)
  (define (iter n d i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter n d (+ i 1))))))
  (iter n d 0))

; k must be 10 or greater to have accuracy to 4 decimal places

; defining the function as iterative instead. Note that we proceed
; from the last term first, which we can use as the base case of
; a running total, starting from (/ (n k) (d k)) 
;
; In the first iteration, we get (/ (n k) (+ (d k) 0)), which is the
; terminating term (/ (n k) (d k))
(define (cont-frac-iterative n d k)
  (define (iter n d total i)
    (if (= i 0)
        total
        (iter n d (/ (n i) (+ (d i) total)) (- i 1))))
  (iter n d 0 k))

; 1.38

(define (e-minus-two k)
  (cont-frac-iterative (lambda (i) 1.0)
                       (lambda (i) (if (= (modulo (+ i 1) 3) 0)
                                       (* 2 (/ (+ i 1) 3))
                                       1))
                       k))

; 1.39

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* 2 i) 1.0))
             k))
