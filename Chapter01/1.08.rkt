#lang racket

(require rackunit)

(define (square x) (* x x))

(define (cubert x)
  (sqrt-iter x 1.0 0))

(define (sqrt-iter x guess prev-guess)
  (if (good-enough? prev-guess guess)
      guess
      (sqrt-iter x (improve x guess) guess)))

(define (good-enough? prev-guess guess)
  (< (abs (- guess prev-guess)) 0.00000001))

(define (improve x guess)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

;TEST

(check-equal? (cubert 27) 3.0)
(check-equal? (cubert 8) 2.0)