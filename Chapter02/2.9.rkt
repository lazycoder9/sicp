#lang racket

(require rackunit)
(require "2.7-2.8.rkt")

(define (radius interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

;TEST
(define x (make-interval 2 4))
(define y (make-interval 4 8))

(check-eq? (radius x) 1)

(define sum (add-interval x y))
(define sum-radius (+ (radius x) (radius y)))

(check-eq? (radius sum) sum-radius)

(define sub (sub-interval x y))

(check-eq? (radius sub) sum-radius)
