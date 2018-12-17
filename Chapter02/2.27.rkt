#lang racket
(require rackunit)

(define (reverse l)
  (define (iter rev-list list)
    (cond ((null? list)
           rev-list)
          ((pair? (car list))
           (iter (cons (iter '() (car list)) rev-list) (cdr list)))
          (else
           (iter (cons (car list) rev-list) (cdr list)))))
  (iter '() l))

;TEST
(define x (list (list 1 2) (list 3 4)))
(define expected (list (list 4 3) (list 2 1)))
(check-equal? (reverse x) expected)
