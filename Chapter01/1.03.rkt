#lang racket

(require rackunit)

(define (solution a b c)
  (define (square x) (* x x))
  (- (+ (square a) (square b) (square c)) (square (min a b c))))

;TEST
(check-equal? (solution 2 2 1) 8)
(check-equal? (solution 1 2 3) 13)
(check-equal? (solution 4 4 5) 41)
(check-equal? (solution 4 3 2) 25)