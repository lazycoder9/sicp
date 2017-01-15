#lang racket

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(add-1 (lambda (f) (lambda (x) x)))

(define one
  (lambda (f) (lambda (x) (f x))))