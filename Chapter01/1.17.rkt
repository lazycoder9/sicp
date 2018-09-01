#lang racket
(require rackunit)

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? x) (= 0 (remainder x 2)))

(define (fast-* a b)
  (cond ((= b 1) a)
        ((even? b) (double (fast-* a (halve b))))
        (else (+ a (fast-* a (- b 1))))))

;TEST
(check-eq? (fast-* 5 6) 30)
(check-eq? (fast-* 12 13) 156)