#lang racket

(require rackunit)

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;TEST

(check-eq? (a-plus-abs-b 4 5) 9)
(check-eq? (a-plus-abs-b 4 -5) 9)
(check-eq? (a-plus-abs-b -4 -6) 2)
