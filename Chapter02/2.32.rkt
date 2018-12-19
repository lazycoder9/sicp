#lang racket
(require rackunit)

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ss) (cons (car s) ss)) rest)))))

;TEST
(define x '(1 2 3))
(define expected '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
(check-equal? (subsets x) expected)
