#lang racket
(require rackunit)

(define (square x) (* x x))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

;TEST
(define x (list (list (list 1 2) 3) (list 4 5) (list 6 (list 7 8))))
(define expected (list (list (list 1 4) 9) (list 16 25) (list 36 (list 49 64))))

(check-equal? (square-tree x) expected)
(check-equal? (square-tree-map x) expected)
