#lang racket

(require rackunit)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1)) (f-recur (- n 2)) (f-recur (- n 3)))))

(define (f-iter n)
  (define (iter a b c counter)
    (if (= counter 0)
        c
        (iter (+ a b c) a b (dec counter))))
  (iter 2 1 0 n))

;TEST

(check-eq? (f-recur 5) 11)
(check-eq? (f-recur 5) (f-iter 5))