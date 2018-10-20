#lang racket
(require rackunit)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (iter x)
    (cond ((= x n) true)
          ((try-it x) (iter (+ x 1)))
          (else false)))
  (iter 1))

; TEST
(check-eq? (andmap fermat-test (list 561 1105 1729 2465 2821 6601)) true)