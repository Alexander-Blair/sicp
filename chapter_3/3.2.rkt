; 3.12

; (cdr x) => (b)

;       __ __    __ __
; x -> |'a|  |->|'b| /|
;      |__|__|  |__|/_|
;                       
;       __ __    __ __
; y -> |'c|  |->|'d| /|
;      |__|__|  |__|/_|
;
;       __ __    __ __    __ __    __ __
; z -> |'a|  |->|'b|  |->|'c|  |->|'d| /|
;      |__|__|  |__|__|  |__|__|  |__|/_|

; w ____
;       |
;       |_ __      __ __
; x -> |'a| -|--> |'b|  |
;      |__|__|    |__||_|
;                     | 
;                _____| 
;                |
;               _| __      __ __
;         y -> |'c|  | -> |'d| /|
;              |__|__|    |__|/_|

; (cdr x) => (b c d)

; 3.13

(define z (make-cycle (list 'a 'b 'c)))
; Take the initial argument list as x:
;       __ __    __ __    __ __
; x -> |'a|  |->|'b|  |->|'c| /|
;      |__|__|  |__|__|  |__|/_|

; After applying make-cycle:
;
;        -------------------------
;       |_ __    __ __    __ __   |
; x -> |'a|  |->|'b|  |->|'c|  |-- 
;      |__|__|  |__|__|  |__|__|

; Calling last-pair on z will recurse forever, as once you get end of the list,
; it has a pointer to the beginning of itself!

(define x (list 'a 'b 'c 'd))
(loop x '())
; temp is '(b c d)
; (set-cdr! x y) => x is now '(a)
(loop '(b c d) '(a))
; temp is '(c d)
; (set-cdr! x y) => '(b a)
(loop '(c d) '(b a))
; temp is '(d)
; (set-cdr! x y) => '(c b a)
(loop '(d) '(c b a))
; temp is '()
; (set-cdr! x y) => '(d c b a)
(loop '() '(d c b a))
; Finished!

; 3.14

; mystery returns a reversed list

; the initial list has its cdr set to the initial y value (an empty list), so that list
; ends up containing only its first element after the function is applied

; 3.16

(count-pairs '(1 2 3)) => 3

(define y (list 1 2))
(define x (cons y (cdr y)))
(count-pairs x) => 4

; There are, however only three pairs:
;       __ __    __ __
; y -> | 1| .|->| 2| /|
;      |_^|__| /|__|/_|
;        |    /
;       _| _ /
; x -> | .| .|
;      |__|__|

(define c (cons 1 2))
(define b (cons c c))
(define a (cons b b))
(count-pairs a) => 7

; There are, however only three pairs:
;             __ __
;       c -> | 1| 2|
;            |_^|__|
;            / |
;          _/ _|
;    b -> | .| .|
;         |_^|__|
;         / |
;       _/ _|
; a -> | .| .|
;      |__|__|
;

; To never reach a count, you could do something as simple as setting two or lists to
; point at each other:
(define y (list 1 2))
(define x (cons y (cdr y)))
(set-cdr! y x)

; 3.17

(define (contains-pair? collection element)
  (define (iter remaining)
    (cond ((null? remaining) false)
          ((eq? element (car remaining)) true)
          (else (iter (cdr remaining)))))
  (if (pair? element)
      (iter collection)
      false))

(define (count-pairs x)
  (let ((found-pairs nil))
    (define (count y)
      (let ((skip? (contains-pair? found-pairs y)))
        (if (or skip? (not (pair? y)))
            0
            (begin
              (set! found-pairs (cons y found-pairs))
              (+ (count (car y))
                 (count (cdr y))
                 1)))))
    (count x)))

; 3.18 & 3.19

; Assumes no nested lists (only cdr-ing through a one dimensional list)
(define (contains-cycle? seq)
  (define (safe-cdr seq)
    (if (pair? seq)
        (cdr seq)
        '()))
  (define (iter a b)
    (cond ((not (or (pair? a)
                    (pair? b))) false)
          ((eq? a b) true)
          (else (iter (safe-cdr a) (safe-cdr (safe-cdr b))))))
  (iter seq (safe-cdr seq)))

; 3.21

(define (print-queue queue)
  (display (front-ptr queue))
  (newline))

; 3.22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (ensure-not-empty! function-name)
      (if (empty?)
          (error function-name " called with an empty queue")))
    (define (empty?) (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (if (empty?)
            (begin (set! front-ptr new-pair)
                   (set! rear-ptr new-pair))
            (begin (set-cdr! rear-ptr new-pair)
                   (set! rear-ptr (cdr rear-ptr))))
        front-ptr))
    (define (delete-queue!)
      (ensure-not-empty! "DELETE!")
      (set! front-ptr (cdr front-ptr))
      front-ptr)
    (define (front)
      (ensure-not-empty! "FRONT")
      (car front-ptr))
    (define (rear)
      (ensure-not-empty! "REAR")
      (car rear-ptr))
    (define (print-queue)
      (display (front)))
    (define (dispatch m)
      (cond ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'front-queue) front)
            ((eq? m 'rear-queue) rear)
            ((eq? m 'empty?) empty?)
            ((eq? m 'print-queue) print-queue)
            (else (error "UNKNOWN OPERATION in MAKE-QUEUE: " m))))
    dispatch))

(define (delete-queue! queue) ((queue 'delete-queue!)))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (front-queue queue) ((queue 'front-queue)))
(define (rear-queue queue) ((queue 'rear-queue)))
(define (print-queue queue) ((queue 'print)))
(define (empty-queue? queue) ((queue 'empty?)))


; 3.23

; Effectively we need to implement a doubly linked list in order to be able to complete
; all operations in O(1) time.

; Inserting a new item at the front involves creating a new item, setting its 'next' pointer
; to the previous front, setting the 'previous' pointer on the old front to the new item,
; and then setting the front of the deque to the new item.

; Inserting a new item at the rear involves creating a new item, setting its 'previous' pointer
; to the previous rear, setting the 'next' pointer on the old rear to the new item,
; and then setting the rear of the deque to the new item.

; Deleting a new item at the front involves finding the 'next' pointer on the current front,
; setting the deque's front to that next pointer, and then setting the 'previous' on the new front
; to nil.

; Deleting a new item at the rear involves finding the 'previous' pointer on the current rear,
; setting the deque's rear to that previous pointer, and then setting the 'next' on the new rear
; to nil.

; Of course, all of the above are for cases where we already have items in the deque. There are
; special cases when the deque is empty and we are inserting (we set front and rear to the same
; thing), or when we are deleting and there's only one item (we set front and rear both to nil).
; Finally, we raise an error if the deque is empty and we try to delete from the front or rear.
(define (make-deque) (list '()))

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

(define (make-deque-item item previous next)
  (list item previous next))
(define (previous ptr) (cadr ptr))
(define (next ptr) (caddr ptr))
(define (set-next! ptr item)
  (if (not (null? ptr))
      (set-car! (cddr ptr) item)))
(define (set-previous! ptr item)
  (if (not (null? ptr))
      (set-car! (cdr ptr) item)))

(define (empty-deque? deque) (null? (car deque)))

(define (insert-into-empty-deque! deque item)
  (let ((item (make-deque-item item '() '())))
    (set-car! deque item)
    (set-cdr! deque item)))

(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque) (insert-into-empty-deque! deque item))
        (else
          (let ((new-item (make-deque-item item '() (front-ptr deque))))
            (set-previous! (front-ptr deque) new-item)
            (set-car! deque new-item)))))

(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque) (insert-into-empty-deque! deque item))
        (else
          (set-next! (rear-ptr deque) (make-deque-item item (rear-ptr deque) '()))
          (set-cdr! deque (next (rear-ptr deque))))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque) (error "FRONT DELETE DEQUE called on empty deque"))
        (else
          (let ((new-front (next (front-ptr deque))))
            (set-car! deque new-front)
            (if (null? new-front)
                (set-cdr! deque '())
                (set-previous! (front-ptr deque) '()))))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque) (error "REAR DELETE DEQUE called on empty deque"))
        (else
          (let ((new-rear (previous (rear-ptr deque))))
            (set-cdr! deque new-rear)
            (if (null? new-rear)
                (set-car! deque '())
                (set-next! (rear-ptr deque) '()))))))

(define (deque-items deque)
  (define (append-next front-ptr)
    (cond ((null? front-ptr) '())
          (else (cons (car front-ptr) (append-next (next front-ptr))))))
  (append-next (front-ptr deque)))

(define (print-deque deque)
  (display (deque-items deque))
  (newline))
