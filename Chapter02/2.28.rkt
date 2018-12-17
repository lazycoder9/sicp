#lang racket
(require rackunit)

(define (fringe l)
  (define (remove-list items acc)
    (cond ((null? items)
           acc)
          ((pair? (car items))
           (remove-list (cdr items) (remove-list (car items) acc)))
          (else
           (remove-list (cdr items) (cons (car items) acc)))))
  (reverse (remove-list l '())))

;TEST
(define x (list 1 2 (list 3 5) (list (list 4 3) 2)))
(define expected (list 1 2 3 5 4 3 2))
(check-equal? (fringe x) expected)
