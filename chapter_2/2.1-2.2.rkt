; 2.1

(define (make-rat n d)
  (let ((g (gcd n d))
        (n-negative? (< n 0))
        (d-negative? (< d 0)))
    (cond ((or (and n-negative? d-negative?)
               (and (not n-negative?) (not d-negative?)))
           (cons (/ (abs n) g) (/ (abs d) g)))
          (n-negative?
           (cons (/ n g) (/ d g)))
          (d-negative?
           (cons (/ (- n) g) (/ (- d) g))))))

; 2.2

(define (average a b) (/ (+ a b) 2))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (cons (average (x-point (start-segment segment))
                 (x-point (end-segment segment)))
        (average (y-point (start-segment segment))
                 (y-point (end-segment segment)))))

; 2.3

; Implementation 1 (using two segments, a top and a left, to represent rectangle
(define (make-rectangle top-segment left-segment)
  (cons top-segment left-segment))
(define (top-segment rectangle)
  (car rectangle))
(define (left-segment rectangle)
  (cdr rectangle))

(define (height rectangle)
  (abs (- (y-point (start-segment (left-segment rectangle)))
          (y-point (end-segment (left-segment rectangle))))))
(define (width rectangle)
  (abs (- (x-point (start-segment (top-segment rectangle)))
          (x-point (end-segment (top-segment rectangle))))))

; Implementation 2 (using two points to represent rectangle, top left and bottom right)
(define (make-rectangle top-left bottom-right)
  (cons top-left bottom-right))
(define (top-left rectangle)
  (car rectangle))
(define (bottom-right rectangle)
  (cdr rectangle))

(define (height rectangle)
  (abs (- (y-point (bottom-right rectangle))
          (y-point (top-left rectangle)))))
(define (width rectangle)
  (abs (- (x-point (bottom-right rectangle))
          (x-point (top-left rectangle)))))

; Perimeter and area methods
(define (perimeter rectangle)
  (+ (* 2 (height rectangle))
     (* 2 (width rectangle))))
(define (area rectangle)
  (* (height rectangle)
     (width rectangle)))

; 2.4

(define (cons x y) 
  (lambda (m) (m x y)))
(define (car z) 
  (z (lambda (p q) p)))
; Definition of cdr:
(define (cdr z) 
  (z (lambda (p q) q)))

; 2.5

(define (power x e)
  (define (iter e total)
    (cond ((= e 0) total)
          (else (iter (- e 1) (* total x)))))
  (iter e 1))

(define (cons x y) 
  (* (power 2 x)
     (power 3 y)))

(define (get-value pair base)
  (define (iter n v)
    (if (not (= (modulo n base) 0))
        v
        (iter (/ n base) (+ v 1))))
  (iter pair 0))

(define (cdr pair) (get-value pair 3))
(define (car pair) (get-value pair 2))

; 2.6

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

; repeated function is defined in chapter 1.4 exercises
(define (+ a b)
  (lambda (f) (lambda (x) ((repeated f b) ((a f) x)))))

; 2.7

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

; 2.8

(define (sub-interval x y)
  (make-interval (abs (- (lower-bound x) 
                         (lower-bound y)))
                 (abs (- (upper-bound x) 
                         (upper-bound y)))))

; 2.9

; If we take two intervals initially defined
(define int-1 (make-interval 6.12 7.48))
(define int-2 (make-interval 4.465 4.935))
; Width of int-1 is 0.68
; Width of int-2 is 0.235

(add-interval int-1 int-2) => (10.585 . 12.415)
; Width of result is 0.915 which equals width of int-1 + width of int-2

(mul-interval int-1 int-2) => (27.3258 . 36.9138)
; Width of result 4.794

; To show that the new width is not purely a function of the initial widths,
; take the following example, where the widths remain the same, but the upper
; and lower bounds are incremented by 1:
(define int-3 (make-interval 7.12 8.48))
(define int-4 (make-interval 5.465 5.935))
; Width of int-1 is 0.68
; Width of int-2 is 0.235

(mul-interval int-3 int-4) => (38.9108 . 50.3288)
; Width of new interval is 5.709. As you can see, the resulting width cannot be
; calculated purely from the width alone


; 2.10

(define (div-interval x y)
  (cond ((= (upper-bound y) (lower-bound y))
         (error "Cannot divide by interval that spans zero"))
        (else (mul-interval x
                            (make-interval
                              (/ 1.0 (upper-bound y))
                              (/ 1.0 (lower-bound y)))))))

; 2.11

(define (mul-interval x y)
  (let* ((low-x (lower-bound x))
         (upper-x (upper-bound x))
         (low-y (lower-bound y))
         (upper-y (upper-bound y))
         (x-negative? (negative? upper-x))
         (y-negative? (negative? upper-y))
         (x-non-negative? (not (negative? low-x)))
         (y-non-negative? (not (negative? low-y)))
         (x-crosses-zero? (and (not x-negative?) (not x-non-negative?)))
         (y-crosses-zero? (and (not y-negative?) (not y-non-negative?))))
  (cond ((and x-non-negative? y-non-negative?) (make-interval (* low-x low-y)
                                                              (* upper-x upper-y)))
        ((and x-negative? y-negative?) (make-interval (* upper-x upper-y)
                                                      (* low-x low-y)))
        ((and x-non-negative? y-negative?) (make-interval (* upper-x low-y)
                                                          (* low-x upper-y)))
        ((and x-negative? y-non-negative?) (make-interval (* low-x upper-y)
                                                          (* low-x low-y)))
        ((and x-crosses-zero? y-non-negative?) (make-interval (* low-x upper-y)
                                                              (* upper-x upper-y)))
        ((and x-non-negative? y-crosses-zero?) (make-interval (* upper-x low-y)
                                                              (* upper-x upper-y)))
        ((and x-negative? y-crosses-zero?) (make-interval (* low-x upper-y)
                                                          (* low-x low-y)))
        ((and x-crosses-zero? y-negative?) (make-interval (* upper-x low-y)
                                                          (* low-x low-y)))
        ((and x-crosses-zero? y-crosses-zero?)
         (let ((p1 (* low-x low-y))
               (p2 (* low-x upper-y))
               (p3 (* upper-x low-y))
               (p4 (* upper-x upper-y)))
           (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))
        (else (error "Invalid interval configuration")))))

; 2.12

(define (make-center-percent c percent-tolerance)
  (let ((tolerance (/ percent-tolerance 100.0)))
    (make-interval (* c (- 1.0 tolerance))
                   (* c (+ 1.0 tolerance)))))


; 2.13

; A rough approximation for the percentage tolerance of the product of two intervals
; is simply the sum of the percentages, assuming that both intervals are positive, and
; only when the percentage tolerances are small

; (define int-1 (make-center-percent 10 1)) => (9.9 . 10.1)
; (define int-2 (make-center-percent 12 1)) => (11.88 . 12.12)
; Product of the two intervals is (117.612 . 122.412)
; percentage tolerance is 1.9998

; If we take the intervals (make-center-percent a 1) and (make-center-percent b 1)
; Lower bound of product is 0.9801ab
; Upper bound of product is 1.0201ab
; Tolerance of new interval is
;   (1.0201ab - 0.9801ab) / 2 == 0.02ab
; Percentage tolerance is
;   100 * tolerance / (upper-bound - tolerance)
;   => 100 * 0.02ab / (1.0201ab - 0.02ab)
;   => 2ab / 1.0001ab, which is 2/1.0001 == 1.9998 (sum of initial percentages, minus a tiny bit)

; As the percentage grows, the divisor will also grow, reducing the accuracy. For example, at 10%
; (make-center-percent a 10) and (make-center-percent b 10)
; Lower bound of product is 0.81ab
; Upper bound of product is 1.21ab
; Tolerance of new interval is
;   (1.21ab - 0.81ab) / 2 == 0.2ab
; Percentage tolerance is
;   100 * tolerance / (upper-bound - tolerance)
;   => 100 * 0.2ab / (1.21ab - 0.2ab)
;   => 20ab / 1.01ab, which is 20/1.01 == 19.8 (sum of initial percentages, minus a bit more)

; And if we then take a 50% percentage tolerance
; Lower bound is 0.25ab
; Upper bound is 2.25ab
; Tolerance of new interval is
;   (2.25ab - 0.25ab) / 2 == ab
; Percentage tolerance is
;   100 * tolerance / (upper-bound - tolerance)
;   => 100 * ab / (2.25ab - ab)
;   => 100ab / 1.25ab, which is 100/1.25 == 80 (sum of initial percentages is now 20% off)

; 2.14

(define int-1 (make-center-percent 10 10))
(define int-2 (make-center-percent 10 1))
(define int-3 (make-center-percent 10 0.1))

(div-interval int-1 int-1) => (0.8181 . 1.2222)
(div-interval int-3 int-3) => (0.9980 . 1.0020)

; When dividing an interval by itself, the smaller the percentage tolerance, the closer
; dividing it by itself equals one i.e. (make-interval 1 1)

; If you take the two formulas, (R1 * R2) / (R1 + R2), and 1 / (1/R1 + 1/R2)
; the second is a derivative of the first, if you divide both sides by R1 * R2
; In the code equivalent of the second version, the formula doesn't seem to work
; due to the fact that 1 / R1 * R1 does not equal 1.


;Hierarchical Data

; 2.17

(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

; 2.18

(define (reverse l)
  (define (iter new old)
    (if (null? old)
        new
        (iter (cons (car old) new) (cdr old))))
  (iter (list) l))

; 2.19

(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))

; The order has no effect, as all possible combinations are calculated

; 2.20

(define (same-parity . list)
  (if (even? (car list))
      (filter list even?)
      (filter list odd?)))

(define (filter items filter-fn)
  (cond ((null? items) nil)
        (else (if (filter-fn (car items))
                  (cons (car items)
                        (filter (cdr items) filter-fn))
                  (filter (cdr items) filter-fn)))))

; 2.21

; Not using map
(define (square-list items)
  (define (square x) (* x x))
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

; Using map
(define (square-list-new items)
  (map (lambda (x) (* x x)) items))

; 2.22

; Effectively, the squares that we calculating are being prepended to the list we are
; building each time

; Given that the first value of answer is nil, when cons is called for the first time,
; you'll end up with the first item squared and nil. The next time, through, the next
; call to cons will be with the second item squared, and the previous pair (first item
; squared and nil). So you'll end up prepending the latest item in the list

; Swapping the arguments to cons also doesn't work as the first call to cons will be with
; nil and the first item squared. Having nil as the first of a pair means the list will end
; up looking like this: (square-list (list 1 2 3 4 5)) => (((((() . 1) . 4) . 9) . 16) . 25)

; 2.23

(define (for-each fn items)
  (cond ((null? items) #t)
        (else (fn (car items))
              (for-each fn (cdr items)))))

; 2.24

; Interpreter gives (1 (2 (3 4)))
;
; Box interpretation:
; (1 (2 (3 4)))
;  _ _     (2 (3 4)) 
; |.|.|     _ _     (3 4)
; |_|_| -> |.|.|     _ _      _ _
;  |       |_|_| -> |.|.| -> |.|/|
;  1        |       |_|_|    |_|_|
;           2        |        |
;                    3        4
;
; Tree interpretation:
; (1 (2 (3 4)))
;     /  \
;    1    . (2 (3 4))
;        / \
;       2   . (3 4)
;             /  \
;            3    4

; 2.25

; To grab the 7 out of the sequence
(define items '(1 3 (5 7) 9))
(cdr (car (cdr (cdr items))))

(define items '((7)))
(car (car items))

(define items '(1 (2 (3 (4 (5 (6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr items))))))))))))

; Using repeated (from exercise 1.43)
(define (repeated f n)
  (define (compose f g) (lambda (x) (f (g x))))
  (define (iter g i)
    (cond ((= i 0) g)
          (else (iter (compose f g) (- i 1)))))
  (iter identity n))

((repeated (lambda (items) (car (cdr items)))
           6)
 items)

; 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))
(append x y) => (1 2 3 4 5 6)
(cons x y) => ((1 2 3) 4 5 6)
(list x y) => ((1 2 3) (4 5 6))

; 2.27

(define (deep-reverse l)
  (define (iter new old)
    (if (null? old)
        new
        (let ((current-item (if (pair? (car old))
                                (deep-reverse (car old))
                                (car old))))
          (iter (cons current-item new) (cdr old)))))
  (iter (list) l))

; 2.28

(define x 
  (list (list 1 2) (list 3 4)))

(define (fringe list)
  (cond ((null? list) nil)
        ((pair? (car list))
         (fringe (append (car list) (cdr list))))
        (else
          (cons (car list) (fringe (cdr list))))))

; 2.29

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


; Part 2

(define (branch-weight branch)
  (if (number? (branch-structure branch))
      (branch-structure branch)
      (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; Part 3

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced? mobile)
  (or (number? mobile)
      (and (= (torque (left-branch mobile))
              (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

; Part 4

; When using cons instead of list to construct mobiles and branches, the only
; changes would be to the right sided selectors:

(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

; 2.30

; Using no higher level abstractions
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
         (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

; Using map abstraction
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square subtree)))
       tree))

; 2.31

(define (tree-map transform-fn tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map transform-fn sub-tree)
             (transform-fn sub-tree)))
       tree))

; 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

; The collection is built from the base case upwards.
; In the example (subsets (list 1 2 3)), we will recurse until
; we reach the terminating case, the list of an empty list (())
;
; We then prepend the final element of the list (which is 3) to each
; list in the base case:
; ((3))
;
; and then append that onto the existing list:
; (() (3))
;
; This step is then repeated. We prepend the next previous list element,
; which is 2, to the already generated lists:
; ((2) (2 3))
;
; and then append that list onto the first:
; (() (3) (2) (2 3))
;
; Repeating the procedure for 1:
; ((1) (1 3) (1 2) (1 2 3))
;
; And appending the new list:
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

; 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              nil
              sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

; 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff sum)
     (+ this-coeff (* x sum)))
   0
   coefficient-sequence))

; 2.35

(define (count-leaves t)
  (accumulate
    (lambda (x y) (inc y))
    0
    (enumerate-tree t)))

; Also works as 

(define (count-leaves t)
  (accumulate
    +
    0
    (map (lambda (x) 1)
         (enumerate-tree t))))

; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

; 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (matrix-vector) (dot-product matrix-vector v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

; 2.38

; For fold-left and fold-right to produce the same output, op should be commutative.
; i.e. multiplication will produce the same result: a * b == b * a 
; division will not since a / b != b / a

; 2.39

; reverse with fold-right
(define (reverse sequence)
  (fold-right
   (lambda (x y) (append y (list x))) nil sequence))

; reverse with fold-left
(define (reverse sequence)
  (fold-left
   (lambda (x y) (cons y x)) nil sequence))

; 2.40

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

; 2.41
; To generate unique tuples of arbitrary size:
(define (unique-tuples size n)
  (accumulate
    (lambda (i combinations)
      (flatmap
        (lambda (tuple)
          (map (lambda (x) (cons x tuple))
               (enumerate-interval i (if (null? tuple)
                                         n
                                         (- (car tuple) 1)))))
        combinations))
    (list nil)
    (enumerate-interval 1 size)))

(define (find-triples-with-sum n s)
  (filter
   (lambda (tuple)
     (= s (accumulate + 0 tuple)))
   (unique-tuples 3 n)))

; 2.42

(define (adjoin-position row-number column-number positions)
  (cons (list column-number row-number) positions))

(define (safe? k positions)
  (define (all-positions-safe? checking-position positions)
    (cond ((null? positions) #t)
          ((accessible? checking-position (car positions)) #f)
          (else (all-positions-safe? checking-position (cdr positions)))))
  (all-positions-safe? (car (filter (lambda (p) (= (car p) k))
                                    positions))
                       positions))

(define (accessible? p1 p2)
  (let ((horizontal-distance (- (car p1) (car p2)))
        (vertical-distance (- (cadr p1) (cadr p2))))
    (and (not (equal? p1 p2))
         (or (= horizontal-distance 0)
             (= vertical-distance 0)
             (= (- (abs horizontal-distance) (abs vertical-distance))
                0)))))

; 2.43

; By interchanging the nested mappings in the queen-cols function, we'd
; end up calling the queen-cols function rows * columns times, instead of
; just once per column. This would suggest that since 6^2 is 36, and 8^2
; is 64, it'd take T * 64 / 36 = 16T/9 (or roughly 1.8 times)

; 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter
                               (- n 1))))
        (below painter
               (beside smaller smaller)))))

; 2.45

(define (split op1 op2)
  (define (repeat painter n)
    (if (= n 0)
        painter
        (let ((smaller (repeat painter (- n 1))))
          (op1 painter
               (op2 smaller smaller)))))
  repeat)

; 2.46

(define (make-vect xcor ycor)
  (cons xcor ycor))

(define (xcor-vect v)
  (car v))

(define (ycor v)
  (cdr v))

(define (add-vect v1 v2)
  (cons (+ (car v1) (car v2))
        (+ (cdr v1) (cdr v2))))

(define (sub-vect v1 v2)
  (cons (- (car v1) (car v2))
        (- (cdr v1) (cdr v2))))

(define (scale-vect v scalar)
  (cons (* (car v) scalar)
        (* (cdr v) scalar)))

; 2.47

; For the following constructor
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
; Selectors would be:
(define (frame-origin frame)
  (car frame))
(define (frame-edge1 frame)
  (cadr frame))
(define (frame-edge2 frame)
  (caddr frame))

; For the following constructor
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
; Selectors would be:
(define (frame-origin frame)
  (car frame))
(define (frame-edge1 frame)
  (cadr frame))
(define (frame-edge2 frame)
  (cddr frame))

; Only difference is the edge2 selector

; 2.48

(define (make-segment start-segment end-segment)
  (cons start-segment end-segment))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

; 2.49

(define (outline-painter frame)
  (let ((far-edge (add-vect (edge1-frame frame) (edge2-frame frame))))
    ((segments->painter (list (make-segment (origin-frame frame) (edge1-frame frame))
                              (make-segment (origin-frame frame) (edge2-frame frame))
                              (make-segment (edge1-frame frame) far-edge)
                              (make-segment (edge2-frame frame) far-edge))) frame)))

(define (draw-x-painter frame)
  (let ((far-edge (add-vect (edge1-frame frame) (edge2-frame frame))))
    ((segments->painter (list (make-segment (origin-frame frame) far-edge)
                              (make-segment (edge1-frame frame) (edge2-frame frame)))) frame)))

(define (draw-diamond frame)
  (let* ((edge1-midpoint          (scale-vect 0.5 (edge1-frame frame)))
         (edge2-midpoint          (scale-vect 0.5 (edge2-frame frame)))
         (opposite-edge1-midpoint (add-vect edge1-midpoint (edge2-frame frame)))
         (opposite-edge2-midpoint (add-vect edge2-midpoint (edge1-frame frame))))
    ((segments->painter (list (make-segment edge1-midpoint edge2-midpoint)
                              (make-segment edge2-midpoint opposite-edge1-midpoint)
                              (make-segment opposite-edge1-midpoint opposite-edge2-midpoint)
                              (make-segment opposite-edge2-midpoint edge1-midpoint))) frame)))

; 2.50

(define (flip-horiz painter)
  (transform-painter
    painter
    (make-vect 1.0 0.0)
    (make-vect 0.0 0.0)
    (make-vect 0.0 1.0)))

(define (rotate180 painter)
  (transform-painter
    painter
    (make-vect 1.0 1.0)
    (make-vect 0.0 1.0)
    (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter
    painter
    (make-vect 0.0 1.0)
    (make-vect 0.0 0.0)
    (make-vect 1.0 1.0)))

; 2.51

; In the same vein as definition of beside:
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom  (transform-painter 
                           painter1
                           (make-vect 0.0 0.0)
                           (make-vect 1.0 0.0)
                           split-point))
          (paint-top (transform-painter
                       painter2
                       split-point
                       (make-vect 1.0 0.5)
                       (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

; In terms of beside + rotation:

(define (below painter1 painter2)
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))
