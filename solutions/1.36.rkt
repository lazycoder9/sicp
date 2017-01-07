#lang racket

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess x)
    (let ((next (f guess)))
      (cond ((close-enough? guess next)
             (newline)
             (display next)
             (display " ")
             (display x)
             next)
            (else
             (newline)
             (display next)
             (display " ")
             (display x)
             (try next (+ x 1))))))
  (try first-guess 1))

(define (average a b) (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define xx (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 100.0))


