; 1.16
(define (square a)
  (* a a))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (super-fast-expt b n)
  (define (expt-iter b n a)
    (cond ((= n 0) 1)
          ((= n 1) (* a b))
          ((even? n) (expt-iter (square b) (/ n 2) a))
          (else (expt-iter b (- n 1) (* a b)))))
  (expt-iter b n 1))

; 1.17

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-multiply a b)
  (cond ((= b 0) 0)
        ((even? b) (fast-multiply (double a) (halve b)))
        (else (+ a (fast-multiply a (- b 1))))))

; 1.18

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (super-fast-multiply a b)
  (define (iter a b c)
    (cond ((= b 0) 0)
          ((= b 1) (+ a c))
          ((even? b) (iter (double a) (halve b) c))
          (else (iter a (- b 1) (+ a c)))))
  (iter a b 0))

; 1.19

(define (fib-b n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) 
           b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (* p p) 
                        (* q q))
                     (+ (* p q) 
                        (* q q) 
                        (* q p))
                     (/ count 2)))
          (else 
           (fib-iter (+ (* b q) 
                        (* a q) 
                        (* a p))
                     (+ (* b p) 
                        (* a q))
                     p
                     q
                     (- count 1)))))
  (fib-iter 1 0 0 1 n))
