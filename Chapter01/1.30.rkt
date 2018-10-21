#lang racket

(require rackunit)

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

; TEST
(check-eq? (sum-integers 1 10) 55)
