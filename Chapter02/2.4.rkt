#lang racket

(require rackunit)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;TEST
(define x (cons 1 2))

(check-eq? (car x) 1)
(check-eq? (cdr x) 2)
