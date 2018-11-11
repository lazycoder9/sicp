#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (repeated f n)
  (lambda (x) (if (= n 1)
                  (f x)
                  ((compose f (repeated f (- n 1))) x))))

