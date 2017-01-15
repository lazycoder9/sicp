#lang racket
(define (compose f g)
  (lambda (x) (f (g x))))

(define (square x) (* x x))

(define (inc x) (+ x 1))

(define (repeated f n)
  (lambda (x) (if (= n 1)
                  (f x)
                  (f ((repeated f (- n 1)) x)))))

(define dx 0.0001)

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
  (try first-guess))

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(((repeated smooth 2) sin) 2)

(define (n-root n x)
  (fixed-point ((repeated average-damp (- n 1))(lambda (y) (/ x (exp y n))))
               1.0))



