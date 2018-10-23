#lang racket

(require rackunit)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (identity x) x)

(define (inc x) (+ x 1))

(define (sum-integers n)
  (sum identity 0 inc n))

(define (factorial n)
  (product identity 1 inc n))

; TEST
(check-eq? (sum-integers 10) 55)
(check-eq? (factorial 5) 120)

