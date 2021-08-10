#lang racket

(provide the-empty-stream
         stream-car
         stream-cdr
         force
         stream-append
         stream-append-delayed
         stream-null?
         interleave-delayed
         stream-map
         stream-flatmap
         flatten-stream
         singleton-stream
         singleton-stream?
         stream-for-each
         cons-stream
         stream-filter
         delay)

(define-syntax delay
  (syntax-rules ()
    ((_ exp) (lambda () exp))))


(define-syntax cons-stream
  (syntax-rules ()
    ((_ a b) (cons a (delay b)))))


(define the-empty-stream '())


(define (stream-car stream) 
  (car stream))


(define (stream-cdr stream) 
  (force (cdr stream)))


(define (force fn)
  (fn))


(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream 
       (stream-car s1)
       (stream-append (stream-cdr s1) s2))))


(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1)
                              delayed-s2))))


(define (stream-null? s)
  (null? s))


(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed 
        (force delayed-s2)
        (delay (stream-cdr s1))))))


(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream 
       (proc (stream-car s))
       (stream-map proc (stream-cdr s)))))


(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))


(define (stream-filter proc s)
  (cond ((stream-null? s) the-empty-stream)
        ((proc (stream-car s))
         (cons-stream (stream-car s) (stream-filter proc (stream-cdr s))))
        (else (stream-filter proc (stream-cdr s)))))


(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream
               (stream-cdr stream))))))


(define (singleton-stream x)
  (cons-stream x the-empty-stream))


(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s))))


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin 
        (proc (stream-car s))
        (stream-for-each proc 
                         (stream-cdr s)))))
