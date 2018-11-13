#lang racket

(require rackunit)

(define (abs x)
  (* x (if (< x 0) -1 1)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (let ((n (/ n g))
          (d (/ d g)))
      (cons (if (negative? d) (* -1 n) n)
            (abs d)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat rat)
  (display (numer rat))
  (display "/")
  (display (denom rat)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))


;TEST
(define a (make-rat 3 -6))
(define b (make-rat 4 10))
(define c (make-rat -1 5))

(check-eq? (equal-rat? (mul-rat a b) c) #true)

