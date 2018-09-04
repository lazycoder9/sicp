#lang racket
(require rackunit)

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;TEST
(check-eq? (fib 0) 0)
(check-eq? (fib 1) 1)
(check-eq? (fib 3) 2)
(check-eq? (fib 5) 5)
(check-eq? (fib 10) 55)
