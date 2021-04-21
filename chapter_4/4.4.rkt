; 4.55

; 1. (supervisor ?person (Bitdiddle Ben))

; 2. (job ?person (accounting . ?role))

; 3. (address ?person (Slumerville . ?where))
;    or if we wanted to break down the address:
;    (address ?person (Slumerville ?street ?house-number))

; 4.56

; 1. (and (supervisor ?person (Bitdiddle Ben))
;         (address ?person ?where))

; 2. (and (salary ?person ?amount)
;         (lisp-value < ?amount 60000))
;         (salary (Bitdiddle Ben) ?number))

; 3. (and (supervisor ?person ?supervisor)
;         (not (job ?supervisor (computer . ?x)))
;         (job ?supervisor ?role))

; 4.57

(rule (can-replace ?person-1 ?person-2)
      (and (not (same ?person-1 ?person-2))
           (job ?person-1 ?role-1)
           (job ?person-2 ?role-2)
           (or (same ?role-1 ?role-2)
               (can-do-job ?role-1 ?role-2))))

; 1. (and (job ?person ?role)
;         (can-replace ?person (Fect Cy D)))

; 2. (and (salary ?person-1 ?amount-1)
;         (salary ?person-2 ?amount-2)
;         (lisp-value < amount-1 amount-2)
;         (can-do-job ?person-1 ?person-2))

; 4.58

(rule (big-shot ?person)
      (supervisor ?person ?boss)
      (job ?person (?division . ?role))
      (job ?boss (?boss-division . ?boss-role))
      (not (same ?division ?boss-division)))

; 4.59

; 1. (meeting ?division (Friday ?time))

; 2.

(rule (meeting-time ?person ?day-and-time)
      (job ?person (?division . ?role))
      (or (meeting ?division ?day-and-time)
          (meeting whole-company ?day-and-time)))

; 3. (and (meeting-time (Hacker Alyssa P) (Wednesday ?time))
;         (meeting ?division (Wednesday ?time)))

; The above assumes she actually wants to know whose meetings she is attending!

; 4.60

; Sorting the entries in alphabetical order in the lives-near rule would prevent this

; 4.61

(?x next-to ?y in (1 (2 3) 4))
; gives:
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

(?x next-to 1 in (2 1 3 1))
; Gives:
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))

; 4.62

(rule (last-pair (?x . ?rest) ?pair)
      (last-pair ?rest ?pair))

(rule (last-pair (?x) (?x)))

; The above rules would not work for (last-pair ?x (3))

; 4.63

(rule (grandsons ?person)
      (son ?father ?person)
      (son ?grandfather ?father))

(rule (mother ?person)
      (son ?father ?person)
      (wife ?father ?mother))

; 4.64

; Since an `or` generates two streams that are to then be merged, and the outranked-by
; statement is now the first clause in the `and` statement, it will cause an infinite
; loop attempting to generate the first set of frames to pass to the second clause in
; the `and`. The ?middle-manager binding is doomed to never be assigned a value (this
; was done previously in the `supervisor` clause, which was then passed recursively
; back to `outranked-by`

; 4.65

; There are four rules that match for Mr Warbucks:
; - He supervises Ben Bitdiddle, who supervises Alyssa P Hacker
; - He supervises Ben Bitdiddle, who supervises Cy D Fect
; - He supervises Ben Bitdiddle, who supervises Lem E Tweakit
; - He supervises Eben Scrooge, who supervises Robert Cratchet

; 4.66

; Where the same result can be repeated, this would of course result in incorrect
; accumulation values. He could deduplicate the resultant frames in order to avoid this.

; 4.70

; When adding the first rule / assertion, an infinite stream would be generated of the
; first element. This means no queries would ever terminate! Any new assertions and rules
; that were added would suffer the same fate, generating an infinite stream of the new
; assertion or rule that was being added!

; 4.71

; We could avoid infinite loops in cases where rules are defined in terms of themselves

; 4.72

; This would allow printing some results where the first stream was infinite

; 4.73

; This different definition would recurse forever

; 4.74

; Pt. 1:
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (f) (not (stream-null? f)))
                             stream)))

; Pt. 2:

; Don't think so, at least it doesn't seem to

; 4.75

; We'll need a way to identify a singleton stream:
(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))

; To implement the uniquely-asserted function, we just need to ensure that for each
; input frame, there is exactly one output frame. Note that for the case where the
; input frame stream is null, we still ensure that we check there is just one matching
; frame:
(define (uniquely-asserted contents frame-stream)
  (define (filter-unique frame)
    (let ((extended-frames (qeval (car contents)
                                  (singleton-stream frame))))
      (if (singleton-stream? extended-frames)
          extended-frames
          the-empty-stream)))
  (if (stream-null? frame-stream)
      (filter-unique frame-stream)
      (stream-flatmap filter-unique frame-stream)))

; To find each person who supervises one person:
(and (supervisor ?x ?boss)
     (unique (supervisor ?anyone ?boss)))

; 4.76

(define (combine-frames-if-consistent f1 f2)
  (cond ((eq? f1 'failed) 'failed)
        ((eq? f2 'failed) 'failed)
        ((null? f2) f1)
        (else (combine-frames-if-consistent (extend-if-consistent (caar f2)
                                                                  (cdar f2)
                                                                  f1)
                                            (cdr f2)))))


; Splitting out conjoin into old 'slow' version, and the faster version. We cannot
; use the new version for any `and` expression containing a lisp-value conjunct.
(define (conjoin conjuncts frame-stream)
  ; When one of the clauses is lisp-value, we cannot process the clauses separately,
  ; as the lisp-value will depend on a value that is not known until we run the prior
  ; query
  (if (includes-lisp-value-clause? conjuncts)
      (slow-conjoin conjuncts frame-stream)
      (fast-conjoin conjuncts frame-stream)))


; Fast conjoin function can take two or more conjuncts and will find all matching frames
; that satisfy all the different conjuncts
(define (fast-conjoin conjuncts frame-stream)
  (define (iter combined-stream evaluated-conjuncts)
    (if (null? evaluated-conjuncts) combined-stream
        (iter (extend-consistent-frames combined-stream (car evaluated-conjuncts))
              (cdr evaluated-conjuncts))))
  (iter (singleton-stream '())
        (map (lambda (conjunct) (qeval conjunct frame-stream))
             conjuncts)))


(define (extend-consistent-frames frame-stream-1 frame-stream-2)
  (define (frame-match? frame) (not (eq? frame 'failed)))
  (if (stream-null? frame-stream-1) frame-stream-2
      (stream-flatmap (lambda (f1)
                        (stream-filter frame-match?
                                       (stream-map
                                         (lambda (f2) (combine-frames-if-consistent f1 f2))
                                         frame-stream-2)))
                      frame-stream-1)))

; 4.77

; In order to implement a promise based approach to making lisp-value and not behave better,
; we have some helper functions:


(define (unresolved-promise? frame)
  (binding-in-frame 'promise frame))

(define (extend-frame-with-promise promise frame)
  (extend 'promise promise frame))

(define (resolve-promise frame)
  (qeval (binding-value (binding-in-frame 'promise frame))
         (singleton-stream (unbind 'promise frame))))

(define (unbind variable frame)
  (define (iter f)
    (cond ((null? f) '())
          ((equal? (caar f) variable)
           (iter (cdr f)))
          (else (cons (car f) (iter (cdr f))))))
  (iter frame))

; When we can resolve an expression (i.e. we can bind all the variables), we execute
; the lisp procedure, otherwise we extend the frame with a promise, to be executed
; when we complete the bindings
(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((resolved-expression (instantiate-lisp-expression call frame)))
        (if resolved-expression
            (if (execute resolved-expression)
                (singleton-stream frame)
                the-empty-stream)
            (singleton-stream (extend-frame-with-promise (cons 'lisp-value call) frame)))))
    frame-stream))

; For negation, the same strategy applies - we only evaluate the expression if the
; variables are bound. Note this current strategy limits the usage of not to only contain
; a single query (i.e. having something like (not (and ...)) wouldn't work). The changes
; required to implement that would be limited to the all-vars-bound? function.
(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((query (negated-query operands)))
        (if (all-vars-bound? query frame)
            (if (stream-null? (qeval query (singleton-stream frame)))
                (singleton-stream frame)
                the-empty-stream)
            (singleton-stream (extend-frame-with-promise (list 'not query) frame)))))
    frame-stream))

(define (all-vars-bound? exp frame)
  (cond ((null? exp) true)
        ((var? (car exp))
         (if (binding-in-frame (car exp) frame)
             (all-vars-bound? (cdr exp) frame)
             false))
        (else (all-vars-bound? (cdr exp) frame))))


; The final change would be wrapping simple-query function in a stream-flatmap, where
; we attempt to resolve any outstanding promises
(define (simple-query query-pattern 
                      frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (unresolved-promise? frame)
          (resolve-promise frame)
          (singleton-stream frame)))
    (stream-flatmap
      (lambda (frame)
        (stream-append-delayed
          (find-assertions query-pattern frame)
          (delay 
            (apply-rules query-pattern frame))))
      frame-stream)))

; Note the fast-conjoin strategy above no longer works once we implement the promises.
; We need to resolve any promises after combining two frames:

(define (extend-consistent-frames frame-stream-1 frame-stream-2)
  (define (frame-match? frame)
    (not (eq? frame 'failed)))
  (if (stream-null? frame-stream-1)
      frame-stream-2
      (stream-flatmap (lambda (f1)
                        (stream-filter
                          frame-match?
                          (stream-flatmap
                            (lambda (f2)
                              (let ((combined (combine-frames-if-consistent f1 f2)))
                                (cond ((eq? combined 'failed)
                                       (singleton-stream combined))
                                      ((unresolved-promise? combined)
                                       (resolve-promise combined))
                                      (else
                                        (singleton-stream combined)))))
                            frame-stream-2)))
                      frame-stream-1)))
