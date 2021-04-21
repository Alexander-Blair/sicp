; 1.40

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 3 3 3) 1) => -2.259921...

; 1.41

(define (double p)
  (lambda (x) (p (p x))))

(((double (double double)) inc) 5) increments by 16

; 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43

(define (repeated-recursive f n)
  (cond ((= n 0) identity)
        (else (compose (repeated-recursive f (- n 1)) f))))

(define (repeated-iterative f n)
  (define (iter g i)
    (cond ((= i 0) g)
          (else (iter (compose f g) (- i 1)))))
  (iter identity n))

; 1.44

(define dx 0.00001)
(define (smooth f)
  (define (average a b c) (/ (+ a b c) 3))
  (lambda (x) (average (f x) (f (+ x dx)) (f (- x dx)))))

; In order to repeatedly smooth:

(define (repeated-smooth f n)
  ((repeated smooth n) f))

; Which would then be used like so:

(define number-of-smooths 5)
((repeated-smooth sqrt number-of-smooths) 4)

; 1.45

(define (nth-root x n)
  (fixed-point
   ((repeated average-damp (floor (log n 2)))
    (lambda (y) (/ x (power y (- n 1)))))
   1.0))

; 1.46

(define (iterative-improve good-enough? improve)
  (define (improve-iter guess)
    (if (good-enough? guess)
        guess
        (improve-iter (improve guess))))
  (lambda (guess) (improve-iter guess)))

(define (sqrt x)
  ((iterative-improve (lambda (guess) (< (abs (- (square guess) x)) 0.001))
                      (lambda (guess) (average guess (/ x guess)))) 1.0))

(define tolerance 0.000000001)

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) first-guess))
