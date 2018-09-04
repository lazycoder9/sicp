#lang racket
(require rackunit)

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (even? x) (= 0 (remainder x 2)))

(define (fast-mult a b)
  (fast-mult-iter a b 0))

(define (fast-mult-iter a b acc)
  (cond ((= b 0) acc)
        ((even? b) (fast-mult-iter (double a) (halve b) acc))
        ((> b 0) (fast-mult-iter a (- b 1) (+ acc a)))
        (else (fast-mult-iter a (+ b 1) (- acc a)))))

;TEST
(check-eq? (fast-mult 10 0) 0)
(check-eq? (fast-mult 10 1) 10)
(check-eq? (fast-mult 10 10) 100)
(check-eq? (fast-mult 10 -10) -100)
(check-eq? (fast-mult -10 10) -100)
