#lang racket

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (lambda (x) (if (= n 1)
                  (f x)
                  ((compose f (repeated f (- n 1))) x))))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
  (try first-guess))

(define (n-root n x)
  (fixed-point ((repeated average-damp (- n 1)) (lambda (y) (/ x (exp y n))))
               1.0))

