#lang sicp

(#%require rackunit)

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 4))

(check-equal? (add-vect v1 v2) (make-vect 4 6))
(check-equal? (sub-vect v1 v2) (make-vect -2 -2))
(check-equal? (scale-vect 3 v1) (make-vect 3 6))
