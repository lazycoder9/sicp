#lang racket
(require rackunit)

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b) (= (remainder b a) 0))

; TEST
(check-eq? (smallest-divisor 199) 199)
(check-eq? (smallest-divisor 1999) 1999)
(check-eq? (smallest-divisor 19999) 7)