#lang racket

(require rackunit)

(define (square x) (* x x))

(define (sqrt x)
  (sqrt-iter x 1.0 0))

(define (sqrt-iter x guess prev-guess)
  (if (good-enough? prev-guess guess)
      guess
      (sqrt-iter x (improve x guess) guess)))

(define (good-enough? prev-guess guess)
  (< (abs (- guess prev-guess)) 0.00000001))

(define (improve x guess)
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

;TEST

(check-equal? (sqrt 4) 2.0)
(check-equal? (sqrt 0.0004) 0.02)