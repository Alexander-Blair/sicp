; 2.53

(list 'a 'b 'c) => (a b c)
(list (list 'george)) => ((george))
(cdr '((x1 x2) (y1 y2))) => ((y1 y2))
(cadr '((x1 x2) (y1 y2))) => (y1 y2)
(pair? (car '(a short list))) => #f
(memq 'red '((red shoes) (blue socks))) => #f
(memq 'red '(red shoes blue socks)) => (red shoes blue socks)

; 2.54

(define (equal? a b)
  (cond ((and (null? a) (null? b))
         #t)
        ((or (null? a) (null? b))
         #f)
        ((and (symbol? a) (symbol? b))
         (eq? a b))
        ((and (list? a) (list? b))
         (and (equal? (car a) (car b))
              (equal? (cdr a) (cdr b))))
        (else #f)))

; 2.55

; 'a is shorthand for (quote a)
; ''a is equivalent to '(quote a), which is a list, of which the first element is 'quote

; 2.56

; Functions for exponentiations:

(define (make-exponentiation base exponent)
  (cond ((= exponent 0) 1)
        ((= exponent 1) base)
        (else
         (list '** base exponent))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base exp)
  (cadr exp))

(define (exponent exp)
  (caddr exp))

; In the cond clause of deriv:

((exponentiation? exp)
 (make-product
   (make-product (exponent exp)
                 (make-exponentiation (base exp)
                                      (make-sum (exponent exp) -1)))
   (deriv (base exp) var)))

; 2.57

; To accommodate two or more terms in make-product and make-sum. Assumes that
; `filter` and `filter-accumulate` are defined
(define (not-number? x) (not (number? x)))

(define (make-product . exp)
  (let ((number-product (filter-accumulate * number? 1 exp))
        (expressions (filter not-number? exp)))
    (cond ((null? expressions) number-product)
          ((= number-product 0) 0)
          ((= number-product 1) 
           (if (null? (cdr expressions))
               (car expressions)
               (cons '* expressions)))
          (else (append (list '* number-product) expressions)))))

(define (make-sum . exp)
  (let ((number-sum (filter-accumulate + number? 0 exp))
        (expressions (filter not-number? exp)))
    (cond ((null? expressions) number-sum)
          ((= number-sum 0)
           (if (null? (cdr expressions))
               (car expressions)
               (cons '+ expressions)))
          (else (append (list '+ number-sum) expressions)))))

; New selectors:

(define (multiplicand p) (apply make-product (cddr p)))
(define (augend s) (apply make-sum (cddr s)))

; 2.58

; Part 1
; To generate list in make-product:
(list m1 '* m2)
; To generate list in make-sum:
(list a1 '+ a2)

; To check if it's a sum in sum? function, we just swap `car` for `cadr`:
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

; To grab the addend, we replace `cadr` with `car:
(define (addend s) (car s))

; Same story for product versions:
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

; 2.59

(define (union-set set1 set2)
  (if (null? set2)
      set1
      (union-set (adjoin-set (car set2) set1)
                 (cdr set2))))

; 2.60

; `adjoin-set` becomes much more efficient (O of 1):
(define (adjoin-set x set)
  (cons x set))

; `element-of-set?` will be implemented the same, but its performance will suffer
; the more elements are added to the set, still with O of n complexity

; For `intersection-set`, we need to make one additional comparison, to check
; that the car of set1 is not contained within the cdr of set1, before adding it
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) 
         '())
        ((and (element-of-set? (car set1) set2)
              (not (element-of-set? (car set1) (cdr set1))))
         (cons (car set1)
               (intersection-set (cdr set1) 
                                 set2)))
        (else (intersection-set (cdr set1) 
                                set2))))

; `union-set` remains reasonably similar, however you need to deduplicate
; across both sets, instead of just one. Note that combiner-fn passed to
; accumulate is the previous definition of adjoin-set!

; Performance of this compared to the original should be the same, O of n^2.
; Worst case it scales with the triangle numbers, so not strictly n^2, but
; there will still be exponential growth.

(define (union-set set1 set2)
  (accumulate
    (lambda (x deduped)
      (if (element-of-set? x deduped)
          deduped
          (cons x deduped)))
    '()
    (append set1 set2)))

; 2.61

(define (adjoin-set x set)
  (cond ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (element-of-set? x set)
  (cond ((or (null? set) (< x (car set))) false)
        ((= x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; 2.62

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1)
                                                (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1)
                                                set2)))
                 ((< x2 x1) (cons x2 (union-set set1
                                                (cdr set2)))))))))

; 2.63

; 1. Both procedures produce the same results for the trees provided. Also
;    tested with completely unbalances trees, both producing the same result
; 2. When inspecting how the functions are expanded, it's worth noting that
;    tree->list-1 ends up making a call to `append` and a call to `cons` once
;    per tree node, whereas tree->list-2 makes a call to `cons` once per node,
;    (and does not use `append`). This seems to imply that tree->list-2 has
;    growth of O(n), and tree->list-1 has a slightly greater growth, dependent
;    on the time complexity of `append`

; 2.64

; Part 1
; ------
; partial-tree receives a list of elements, along with the number of elements to include
; in the tree. The list is effectively split into two (the left and right branches), also
; leaving one element to be the entry for the given tree node. To derive the left and right
; branches, those elements are recursively passed to `partial-tree`, which continues to
; recurse until it hits the base case (number of elements 0).

; Tree produced for '(1 3 5 7 9 11):
;    5
;   / \
;  /   \
; 1     9
;  \   / \
;   3 7  11
;
; In list form this looks like (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

; Part 2
; ------
; Growth is O of n. When comparing a list of size 6 to a list of size 12, `partial-tree`
; function is invoked 25 times for a list of size 12, compared to 13 times for a list of
; size 6.

; 2.65

; `intersect-ordered-list` uses the previous implementation of `intersection-set`, which
; operated on the previous representation (an ordered list).
(define (intersection-set set1 set2)
  (list->tree
    (intersect-ordered-list (tree->list-2 set1)
                            (tree->list-2 set2))))

; Similarly, `union-ordered-list` uses the previous implementation of `union-set`, whcih
; operated on the previous representation (an ordered list).
(define (union-set set1 set2)
  (list->tree
    (union-ordered-list (tree->list-2 set1)
                        (tree->list-2 set2))))

; 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key 
                 (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key
            (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((< (key (entry set-of-records))
            given-key)
         (lookup given-key (right-branch set-of-records)))))

; 2.67

(define sample-tree
  (make-code-tree 
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree 
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))
; Visual representation of the sample-tree:

;    {A B D C} 8
;     /      \
;   A(4)   {B D C} 4
;           /   \
;         B(2) {D C} 2
;               /  \
;              D(1) C(1)

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
; Separates into 0, 110, 0, 10, 10, 111, 0
; ADABBCA

; 2.68
(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((element-of-set? symbol (symbols (left-branch tree)))
         (cons 0 (encode-symbol symbol (left-branch tree))))
        ((element-of-set? symbol (symbols (right-branch tree)))
         (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "Symbol not found in tree!"))))

; Where element-of-set? is the definition we used to check presence in an unordered set:
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


; 2.69

(define (successive-merge leaf-set)
  (if (< (length leaf-set) 2)
      (car leaf-set)
      (successive-merge
        (cons
          (make-code-tree (cadr leaf-set) (car leaf-set))
          (cddr leaf-set)))))

; 2.70

; Frequency pairs:
(define rock-freq-pairs '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

; Input message:
(define rock-input
  (append
   '(GET A JOB SHA NA NA NA NA NA NA NA NA)
   '(GET A JOB SHA NA NA NA NA NA NA NA NA)
   '(WAH YIP YIP YIP YIP)
   '(YIP YIP YIP YIP YIP)
   '(SHA BOOM)))

; To generate the bits:
(encode rock-input (generate-huffman-tree rock-freq-pairs))
; Gives: (1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 0 1 1 1 1 1 1 0)
; Organised into the relevant lines:
; '(1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0
;   1 1 1 1 0 1 1 1 0 1 1 1 1 1 0 1 1 0 0 0 0 0 0 0 0 0
;   1 1 1 1 1 1 1 1 0 1 0 1 0 1 0
;   1 1 1 1 1 1 1 1 0 1 0 1 0 1 0
;   1 1 0 1 1 1 1 1 1 0)
; Took 87 bits to represent the lyrics

; Using a fixed length code for the 8 symbol alphabet, we'd need three bits for each
; 'word' in the lyrics, with A being represented as 000, through to WAH being represented
; as 111.
;
; There are 36 words in the lyrics, and if each is represented with three bits each, it
; would take 36 * 3 bits in total, which is 108 bits.

; There is a better successive-merge, which uses `adjoin-set` in place of cons, which results
; in a better balanced tree. Using `cons` means that the right side of the tree becomes very,
; very expensive to traverse

(define (successive-merge leaf-set)
  (if (< (length leaf-set) 2)
      (car leaf-set)
      (successive-merge
        (adjoin-set
          (make-code-tree (car leaf-set) (cadr leaf-set))
          (cddr leaf-set)))))

; Using this version of the function, it takes only 84 bits (a saving of three)

; 2.71

; When relative frequencies of the symbols are 1, 2, 4, 2^(n-1)
; For n = 5, tree looks like:
;     {a b c d e} (31)
;      /       \
;     e(16)    {a b c d} (15)
;               /      \
;              d(8)    {a b c} (7)
;                       /   \
;                      c(4)  {a b} (3)
;                            /  \
;                          b(2)  a(1)

; For n = 10, tree looks like:
;     {a..j} (1023)
;      /   \
;    j(512) {a..i} (511)
;           /    \
;         i(256)  {a..h} (255)
;                 /    \
;               h(128)  {a..g} (127)
;                       /   \
;                     g(64)  {a..f} (63)
;                             /   \
;                           f(32)  {a..e} (31)
;                                 /     \
;                                e(16)  {a b c d} (15)
;                                        /      \
;                                       d(8)    {a b c} (7)
;                                                /   \
;                                               c(4)  {a b} (3)
;                                                     /  \
;                                                   b(2)  a(1)

; For the most frequent symbol we'll need just one bit, and for the least frequent symbol,
; n - 1 bits.

; 2.72

; Looking up the most frequently used element:

; i.e. for when n = 5
; - (leaf? tree)
;    1 step
; - Grabbing the symbols of left branch i.e. (symbols (left-branch tree))
;    2 steps
; - (element-of-set? 'e '(e)) where '(e) is the list of symbols in left branch of the tree;
;     it's the first element in the list
;    1 step
; - (cons 0 (encode-symbol symbol (left-branch tree))) call to cons and left-branch
;    2 steps
; Second iteration:
; - (leaf? tree) returns true, terminates
;    1 step
;
; Overall, has constant time - takes 7 steps, which gives it O(1) growth.

; Looking up the least frequently used element:
; Also for when n = 5
; - (leaf? tree) 1 step
; - Grabbing the symbols of left branch i.e. (symbols (left-branch tree))
;   2 steps
; - (element-of-set? 'a '(e)) where '(e) is the list of symbols in left branch of the tree;
;     it recurses once (after which it's empty and returns false)
;   2 steps
; - Grabbing the symbols of right branch i.e. (symbols (right-branch tree))
;   2 steps
; - (element-of-set? 'a '(a b c d)) first iteration finds match
;   1 step
; - (cons 1 (encode-symbol symbol (right-branch tree))) cons is a step, as is right-branch lookup
;   2 steps
;  9 steps for first iteration
;
; There are n - 1 iterations that do not yield the answer. Also note that the element-of-set?
; call is lucky to only take a single step, as the least frequently used element is always
; at the start of the list.
;
; For the final iteration (where the leaf is found):
; (leaf? tree) returns true, terminated
;   1 step

; If we put this all together, we end up with 9(n - 1) + 1, which has linear growth - O(n)
