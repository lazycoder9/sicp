#lang racket
(require rackunit)
(require "2.17-18.rkt")

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (append answer
                  (list ((lambda (x) (* x x)) (car things)))))))
  (iter items '()))

;TEST
(check-equal? (list 1 4 9 16 25) (square-list (list 1 2 3 4 5)))
