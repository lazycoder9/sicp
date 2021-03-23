#lang racket

(require rackunit)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))

(define s (make-segment v1 v2))
(check-equal? (start-segment s) v1)
(check-equal? (end-segment s) v2)
