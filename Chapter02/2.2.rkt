#lang racket

(require rackunit)

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-point seg) (car seg))

(define (end-point seg) (cdr seg))

(define (midpoint-segment seg)
  (let ((start (start-point seg))
        (end (end-point seg)))
    (make-point (/ (+ (x-point start)
                      (x-point end))
                   2)
                (/ (+ (y-point start)
                      (y-point end))
                   2))))
;TEST
(define x (make-point 2 4))
(define y (make-point -2 -4))

(check-eq? (x-point x) 2)
(check-eq? (y-point y) -4)

(define seg (make-segment x y))
(define mid (midpoint-segment seg))

(check-eq? (x-point mid) 0)
(check-eq? (y-point mid) 0)
