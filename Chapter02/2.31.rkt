#lang racket
(require rackunit)

(define (square x) (* x x))

(define (tree-map proc tree)
  (cond ((null? tree) '())
        ((not (pair? tree))
              (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-tree tree) (tree-map square tree))

;TEST
(define x (list (list (list 1 2) 3) (list 4 5) (list 6 (list 7 8))))
(define expected (list (list (list 1 4) 9) (list 16 25) (list 36 (list 49 64))))

(check-equal? (square-tree x) expected)
