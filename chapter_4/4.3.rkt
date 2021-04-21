; 4.35
(define (an-integer-between x y)
  (require (<= x y))
  (amb x (an-integer-between (+ x 1) y)))

; 4.36

; Say if we did (a-pythagorean-triple-starting-from 1), if we're exploring all possibilities,
; possibilities, i will be set to 1, j will be set to 1, and then k will increase forever,
; attempting to find a triple with i of 1, and j of 1

; To make it work properly, we need to enforce some basic constraints:
; - i < j
; - j < k
; - j < i^2 / 2
;   - Given that given any j, k must be at least j + 1, you can work out the difference
;     between j and k to be at minimum 2j + 1. This means that i^2 must be greater than
;     or equal to 2j + 1, or put another way, i^2 must be greater than 2j, or j must be
;     less than i^2 / 2
; - k < i + j
;   - This fact can easily be seen by having a look at a right angle triangle - the longest
;     side cannot be longer than both other sides combined
(define (a-pythagorean-triple-starting-from n)
  (let ((i (an-integer-starting-from n)))
    (let ((j (an-integer-between (+ i 1) (/ (* i i) 2))))
      (let ((k (an-integer-between (+ j 1) (+ i j))))
        (require (= (+ (* i i) (* j j)) 
                    (* k k)))
        (list i j k)))))

; 4.37

; It still explores all the possible values of i and j, and performs all the squaring
; operations that were performed in the previous method. The best way to limit the
; possibilities searched would be to minimise the search itself, as shown above

; 4.38

; There are 5 solutions when that constraint is removed

; 4.39

; Before the distinct requirement, there are 3125 (5^5) combinations. After, there are
; 120 combinations (5 factorial).

; 4.40

; Measured the time taken to run this new function 100 times, which came out to about
; ~50ms, whereas the original took over 1 second. We could go one further and remove the
; values we know we don't want from the amb itself, but this didn't make much of a
; difference to the runtime:
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (excludes? (list 1 baker) cooper))
      (let ((fletcher (amb 1 2 3 4 5)))
        (require (excludes? (list 1 5 baker cooper) fletcher))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (excludes? (list baker cooper fletcher) miller))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require (excludes? (list baker cooper fletcher miller) smith))
            (require (not (= (abs (- smith fletcher)) 1)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

; 4.41

(define (multiple-dwelling-scheme)
  (define (failed-condition? baker cooper fletcher miller smith)
    (or (= baker 5) (= cooper 1) (= fletcher 1) (= fletcher 5)
        (not (distinct? (list baker cooper fletcher miller smith)))
        (= (abs (- smith fletcher)) 1)
        (= (abs (- fletcher cooper)) 1)
        (< miller cooper)))
  (define (find-next combinations baker cooper fletcher miller smith)
    (if (null? baker)
        combinations
        (let ((next-selection (cond ((< smith 5)    (list baker cooper fletcher miller (+ smith 1)))
                                    ((< miller 5)   (list baker cooper fletcher (+ miller 1) 1))
                                    ((< fletcher 5) (list baker cooper (+ fletcher 1) 1 1))
                                    ((< cooper 5)   (list baker (+ cooper 1) 1 1 1))
                                    ((< baker 5)    (list (+ baker 1) 1 1 1 1))
                                    (else           (list nil nil nil nil nil))))
              (next-combinations (if (failed-condition? baker cooper fletcher miller smith)
                                     combinations
                                     (cons (list (list 'baker baker)
                                                 (list 'cooper cooper)
                                                 (list 'fletcher fletcher)
                                                 (list 'miller miller)
                                                 (list 'smith smith))
                                           combinations))))
          (apply find-next (cons next-combinations next-selection)))))
  (find-next '() 1 1 1 1 1))

; 4.42

; Helper function:
(define (exclusive-or a b)
  (and (or a b)
       (not (and a b))))

(define (liars)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (require (exclusive-or (= kitty 2) (= betty 3)))
    (require (exclusive-or (= ethel 1) (= joan 2)))
    (require (exclusive-or (= joan 3) (= ethel 5)))
    (require (exclusive-or (= kitty 2) (= mary 4)))
    (require (exclusive-or (= mary 4) (= betty 1)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

; By filtering out possibilities as early as possible, we can make the same function
; run in a third of the time:
(define (liars)
  (let ((betty (amb 1 2 3 4 5)))
    (let ((ethel (amb 1 2 3 4 5)))
      (require (not (= ethel betty)))
      (let ((joan (amb 1 2 3 4 5)))
        (require (excludes? (list betty ethel) joan))
        (let ((kitty (amb 1 2 3 4 5)))
          (require (excludes? (list betty ethel joan) kitty))
          (let ((mary (amb 1 2 3 4 5)))
            (require (excludes? (list betty ethel joan kitty) mary))
            (require (exclusive-or (= kitty 2) (= betty 3)))
            (require (exclusive-or (= ethel 1) (= joan 2)))
            (require (exclusive-or (= joan 3) (= ethel 5)))
            (require (exclusive-or (= kitty 2) (= mary 4)))
            (require (exclusive-or (= mary 4) (= betty 1)))
            (list (list 'betty betty)
                  (list 'ethel ethel)
                  (list 'joan joan)
                  (list 'kitty kitty)
                  (list 'mary mary))))))))

; The only answer is ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

; 4.43

; Without making any optimisations:
(define (yachts)
  (define (remaining-fathers)
    (amb 'col-downing 'dr-parker 'mr-hall))
  (define (yacht-owner daughter)
    (car daughter))
  (define (father daughter)
    (cadr daughter))
  (let ((gabrielle (list 'sir-b-hood (remaining-fathers)))
        (lorna (list 'mr-moore (remaining-fathers)))
        (melissa (list 'mr-downing 'sir-b-hood))
        (mary-ann (list 'dr-parker 'mr-moore))
        (rosalind (list 'mr-hall (remaining-fathers))))
    (define (daughter father-name)
      (cond ((eq? (father gabrielle) father-name) gabrielle)
            ((eq? (father lorna) father-name) lorna)
            ((eq? (father melissa) father-name) melissa)
            ((eq? (father mary-ann) father-name) mary-ann)
            ((eq? (father rosalind) father-name) rosalind)))
    (require (distinct? (map father
                             (list gabrielle lorna melissa mary-ann rosalind))))
    (require (distinct? (map yacht-owner
                             (list gabrielle lorna melissa mary-ann rosalind))))
    (require (eq? (yacht-owner (daughter 'dr-parker))
                  (father gabrielle)))
    (list (list 'gabrielle gabrielle)
          (list 'lorna lorna)
          (list 'melissa melissa)
          (list 'mary-ann mary-ann)
          (list 'rosalind rosalind))))

; Attempting to weed out more possibilities earlier in the process:
(define (yachts)
  (define (fathers)
    (amb 'col-downing 'dr-parker 'mr-hall))
  (define (yacht-owner daughter)
    (car daughter))
  (define (father daughter)
    (cadr daughter))
  (let ((melissa (list 'mr-downing 'sir-b-hood))
        (mary-ann (list 'dr-parker 'mr-moore)))
    (let ((gabrielle (list 'sir-b-hood (fathers))))
      (require (distinct? (map father (list melissa mary-ann gabrielle))))
      (require (distinct? (map yacht-owner (list melissa mary-ann gabrielle))))
      (let ((lorna (list 'mr-moore (fathers))))
        (require (distinct? (map father (list melissa mary-ann gabrielle lorna))))
        (require (distinct? (map yacht-owner (list melissa mary-ann gabrielle lorna))))
        (let ((rosalind (list 'mr-hall (fathers))))
          (require (distinct? (map father (list gabrielle lorna melissa mary-ann rosalind))))
          (require (distinct? (map yacht-owner (list gabrielle lorna melissa mary-ann rosalind))))
          (define (daughter father-name)
            (cond ((eq? (father gabrielle) father-name) gabrielle)
                  ((eq? (father lorna) father-name) lorna)
                  ((eq? (father melissa) father-name) melissa)
                  ((eq? (father mary-ann) father-name) mary-ann)
                  ((eq? (father rosalind) father-name) rosalind)))
          (require (eq? (yacht-owner (daughter 'dr-parker))
                        (father gabrielle)))
          (list (list 'gabrielle gabrielle)
                (list 'lorna lorna)
                (list 'melissa melissa)
                (list 'mary-ann mary-ann)
                (list 'rosalind rosalind)))))))

; When we optimise, for the case when Mary Ann's father is not known, we check 164
; different possibilities, whereas in the original version, we check 256. It has a
; fairly minor effect on the runtime of the function. In the original problem it
; actually searches slightly fewer combinations (however runtime looks to be the same)

; Four solutions if we don't know Mary-Ann's surname

; 4.44

; Helper functions to help determine if a given square is safe or not:
(define (column queen)
  (car queen))

(define (row queen)
  (cdr queen))

(define (safe? proposed-queen placed-queens)
  (cond ((null? placed-queens) true)
        ((accessible? proposed-queen (car placed-queens)) false)
        (else (safe? proposed-queen (cdr placed-queens)))))

(define (accessible? queen-1 queen-2)
  (let ((horizontal-distance (- (column queen-1)
                                (column queen-2)))
        (vertical-distance (- (row queen-1)
                              (row queen-2))))
    (or (= horizontal-distance 0)
        (= vertical-distance 0)
        (= (abs horizontal-distance) (abs vertical-distance)))))

; The function itself:

(define (queens board-size)
  (define (add-next placed-queens column-number)
    (if (> column-number board-size)
        (reverse placed-queens)
        (let ((next-queen (cons column-number (integers-up-to board-size))))
          (require (safe? next-queen placed-queens))
          (add-next (cons next-queen placed-queens) (+ column-number 1)))))
  (add-next '() 1))

; 4.45

; - Professor, alongside the cat, is lecturing to the student, in the class
; - professor, alongside the cat, is lectures to the only student in the class
; - Professor is lecturing the student that is identifiable in the class by the fact he has a cat
; - Professor is lecturing the only student in the class, who has a cat
; - Professor is lecturing the student, in the class which is identifiable by the cat's presence

; 4.46

; If we're trying to parse language, it wouldn't be much good if we did not evaluate
; from left to right (unless you're speaking Hebrew or Arabic!)

; 4.47

; The code loops forever as we have an infinite chain of ambs. This behaviour would not be
; fixed by changing the order of the expressions.

; 4.48

(define (parse-simple-noun-phrase)
  (define (maybe-append-adjective article)
    (amb article
         (maybe-append-adjective
           (list 'article-with-adjective
                 article
                 (parse-word adjectives)))))
  (list 'simple-noun-phrase
        (maybe-append-adjective (parse-word articles))
        (parse-word nouns)))

; 4.49

; We can amend parse-word to randomly pick a word from the list provided:
(define (parse-word word-list)
  (list (car word-list) (pick-random (cdr word-list))))

(define (pick-random seq)
  (let ((index (random (length seq))))
    (define (iter remaining n)
      (if (= index n)
          (car remaining)
          (iter (cdr remaining) (+ n 1))))
    (iter seq 0)))

; If you then call (parse-sentence), and then (amb) after, you will generate a sentence,
; albeit with limited sense:

(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase (verb-phrase (verb sleeps) (prep-phrase (prep with) (simple-noun-phrase (article a) (noun class)))) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun boots))))
  (prep-phrase (prep with) (simple-noun-phrase (article a) (noun student)))))

; 4.50

; Iterates through items to a random index, after which it creates a list
; where the item at the given index goes to the front of the list. The order
; of the rest of the list is not guaranteed to stay the same
(define (move-random-item-to-front items)
  (define (iter seq seen-items random-i i)
    (if (eq? random-i i)
        (cons (car seq)
              (append seen-items (cdr seq)))
        (iter (cdr seq) (cons (car seq) seen-items) random-i (+ i 1))))
  (if (eq? (length items) 1)
      items
      (iter items '() (random (- (length items) 1)) 0)))

; To implement the ramb, all I have done is move a random choice to the front of the list
; before executing the car of the list:
(define (analyze-ramb exp)
  (let ((cprocs
          (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((randomise-first (move-random-item-to-front choices)))
              ((car randomise-first)
               env
               succeed
               (lambda ()
                 (try-next (cdr randomise-first)))))))
      (try-next cprocs))))

; The only problem with the above solution is that if you define a `ramb` recursively,
; the order will not be changed at all. If you had (ramb 1 2 3), that will give you back
; the numbers in a random order, however with (ramb 1 (ramb 2 (ramb 3))) etc., it does
; not work. Especially since you can also define infinite ambs or rambs - in that case,
; there is no way to select an item in a fair way.

; 4.51

; The extra clause in the cond statement:
((permanent-assignment? exp) 
 (analyze-permanent-assignment exp))

; Primary difference is that the function is much simpler, since you don't have to create
; a new function to both unset the variable and then call the fail function:
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze 
                (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value!
                var 
                val 
                env)
               (succeed 
                'ok
                fail2))
             fail))))

; If we used `set!` in the example code provided, once the amb tree had been exhausted, it
; would go back to zero.

; 4.52

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-test exp)
  (cadr exp))

(define (fail-value exp)
  (caddr exp))

; Analyzing the expression. Note that I've made it so that if you call try-again, it will
; continue to return the caught failure value, not continue the fail chain. This could easily
; be changed by replacing the recursive call in the let declarations to fail2 with fail:
(define (analyze-if-fail exp)
  (let ((test (analyze (if-fail-test exp)))
        (catch (analyze (fail-value exp))))
    (lambda (env succeed fail)
      (letrec ((fail2 (lambda () (catch env succeed fail2))))
        (test env
              succeed
              fail2)))))

; 4.53

; All the prime sum pairs will be found and returned when the amb tree is exhausted:
; ((8 35) (3 110) (3 20))

; 4.54

(define (analyze-require exp)
  (let ((pproc (analyze 
                 (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
