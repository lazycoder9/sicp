#lang racket
(require rackunit)

(define (square-list items)
  (if (null? items)
      '()
      (cons ((lambda (x) (* x x)) (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

;TEST
(define l (list 1 2 3 4 5))
(define expected (list 1 4 9 16 25))
(check-equal? expected (square-list l))
(check-equal? expected (square-list-map l))

