#lang racket

(define (square x) (* x x))

(define (smallest-divisor n)
  (define (iter divisor)
    (cond ((> (square divisor) n) 1)
          ((= (remainder n divisor) 0) divisor)
          (else (iter (+ divisor 1)))))
  (iter 2))

(define (prime? n)
  (= (smallest-divisor n) 1))
  
; Result of function will be list like (num1 time1 num2 time2)
(define (search-for-primes start prime-count)
  (define (iter current time count acc)
    (cond ((= count prime-count) acc)
          ((prime? current) (iter (+ current 2)
                                  (current-inexact-milliseconds)
                                  (+ count 1)
                                  (append acc (list current (- (current-inexact-milliseconds) time)))))
          (else (iter (+ current 2)
                      (current-inexact-milliseconds)
                      count
                      acc))))
  (iter (+ start 1) (current-inexact-milliseconds) 0 '()))
