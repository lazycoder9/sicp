#lang racket

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
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
  
; Result of function will be list like (num1 time1 num2 time2)
(define (search-for-primes start prime-count)
  (define (iter current time count acc)
    (cond ((= count prime-count) acc)
          ((fast-prime? current 3) (iter (+ current 2)
                                         (current-inexact-milliseconds)
                                         (+ count 1)
                                         (append acc (list current (- (current-inexact-milliseconds) time)))))
          (else (iter (+ current 2)
                      (current-inexact-milliseconds)
                      count
                      acc))))
  (iter (+ start 1) (current-inexact-milliseconds) 0 '()))
