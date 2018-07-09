#lang racket

(require rackunit)

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

;TEST

(check-eq? (A 1 10) 1024)
(check-eq? (A 2 4) 65536)
(check-eq? (A 3 3) 65536)

(check-eq? (f 10) 20)   ; f(x) = 2x
(check-eq? (g 10) 1024) ; g(x) = x^2
(check-eq? (h 3) 16)    ; h(x) = 2^h(x - 1)
(check-eq? (k 5) 125)   ; k(x) = 5x^2