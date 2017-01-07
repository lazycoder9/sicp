#lang racket

(define (make-point x y)
  (cons x y))

(define (square x) (* x x))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average a b) (/ (+ a b) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let ((avr-x (average (x-point (start-segment segment)) (x-point (end-segment segment))))
        (avr-y (average (y-point (start-segment segment)) (y-point (end-segment segment)))))
    (cons avr-x avr-y)))

(define (length-segment segment)
  (sqrt (+ (square (- (x-point (start-segment segment)) (x-point (end-segment segment))))
           (square (- (y-point (start-segment segment)) (y-point (end-segment segment)))))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (make-rectangle left-up-corner width height)
  (cons left-up-corner (cons width height)))

(define (square-rec rectangle)
  (* (car (cdr rectangle)) (cdr (cdr rectangle))))

(define (perimeter-rec rectangle)
  (* 2 (+ (car (cdr rectangle)) (cdr (cdr rectangle)))))

(print-point (make-point 1 2))

(define a (make-point 0 0))

(define b (make-point 3.0 4.0))

(define l (make-segment a b))

(define mid (midpoint-segment l))

(print-point mid)

(length-segment l)

(define rec (make-rectangle (make-point 5 5) 5 5))

(square-rec rec)

(perimeter-rec rec)
