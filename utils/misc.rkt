#lang racket

(provide (all-defined-out))

(define (square x) (* x x))

(define (smallest-divisor n)
  (define (iter divisor)
    (cond ((> (square divisor) n) 1)
          ((= (remainder n divisor) 0) divisor)
          (else (iter (+ divisor 1)))))
  (iter 2))

(define (prime? n)
  (= (smallest-divisor n) 1))
