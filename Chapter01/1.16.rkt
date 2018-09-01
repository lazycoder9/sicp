#lang racket
(require rackunit)

(define (square x) (* x x))

(define (even? n)
  (= 0 (remainder n 2)))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

;TEST
(check-eq? (fast-expt 2 1) 2)
(check-eq? (fast-expt 2 3) 8)
(check-eq? (fast-expt 2 6) 64)

