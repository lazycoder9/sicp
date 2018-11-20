#lang racket

(require "2.7-2.8.rkt")

(define (div-interval x y)
  (if (or (zero? (upper-bound x))
          (zero? (lower-bound y)))
    (error "Division by zero")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))


