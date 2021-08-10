#lang sicp

; Node constructors and accessors
(define (make-tree-node value left right)
  (list left right value))

(define (left-node node)
  (car node))
(define (right-node node)
  (cadr node))
(define (node-value tree)
  (caddr tree))
(define (increment-node-value tree n)
  (set-car! (cddr tree) (+ (node-value tree) n)))

; Range helper methods
(define (includes? range1-start range1-end range2-start range2-end)
  (<= range1-start range2-start range2-end range1-end))

(define (overlaps? range1-start range1-end range2-start range2-end)
  (or (<= range1-start range2-start range1-end)
      (<= range2-start range1-start range2-end)))

; Initialize a tree with the given number of leaf nodes
(define (make-tree leaf-nodes)
  (let ((next-leaf-nodes (round (/ leaf-nodes 2))))
    (if (= leaf-nodes 1)
        (make-tree-node 0 '() '())
        (make-tree-node 0 (make-tree next-leaf-nodes) (make-tree next-leaf-nodes)))))

(define (power-of-two-greater-than-or-equal n)
  (expt 2 (inexact->exact (ceiling (log n 2)))))

(define (segment-tree size)
  (let ((tree (make-tree (power-of-two-greater-than-or-equal size))))
    (define (next-right-node-start tree-node-start tree-node-end)
      (round (/ (+ tree-node-start tree-node-end 1) 2)))
    (define (add value start end)
      (define (iter node node-start node-end)
        (cond ((includes? start end node-start node-end)
               (increment-node-value node value))
              ((and (not (= node-start node-end))
                    (overlaps? start end node-start node-end))
               (let ((right-node-start (next-right-node-start node-start node-end)))
                 (iter (left-node node) node-start (- right-node-start 1))
                 (iter (right-node node) right-node-start node-end)))))
      (cond ((< start 1) (error "Start position is out of bounds: " start))
            ((> end size) (error "End position is out of bounds: " end))
            (else (iter tree 1 size))))
    (define (get-value n)
      (define (traverse node tree-node-start tree-node-end)
        (cond ((= tree-node-end tree-node-start n)
               (node-value node))
              ((>= tree-node-end n tree-node-start)
               (let ((right-node-start (next-right-node-start tree-node-start tree-node-end)))
                 (+ (node-value node)
                    (traverse (left-node node) tree-node-start (- right-node-start 1))
                    (traverse (right-node node) right-node-start tree-node-end))))
              (else 0)))
      (traverse tree 1 size))
    (define (dispatch m)
      (cond ((eq? m 'add) add)
            ((eq? m 'get-value) get-value)
            ((eq? m 'tree) tree)
            (else (error "Unknown operation in segment-tree: " m))))
    dispatch))

(define (add-value tree value start end)
  ((tree 'add) value start end))

(define (get-value tree position)
  ((tree 'get-value) position))

;--------------------------
(define t (segment-tree 16))

(add-value t 10 2 11)
(add-value t 5 10 14)

(define (get-values start end)
  (if (> start end)
      '()
      (cons ((t 'get-value) start)
            (get-values (+ start 1) end))))