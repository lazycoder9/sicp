#lang racket

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat a b)
  (let ((g (gcd a b)))
    (cond ((or (and (< b 0) (> a 0)) (and (< a 0) (> b 0)))
           (cons (* (/ a g) (- 1)) (* (/ b g) (- 1))))
          (else 
           (cons (/ a g) (/ b g))))))

(define (numer pair) (car pair))

(define (denom pair) (cdr pair))

(define x (make-rat 1 (- 2)))

(define y (make-rat 1 6))

(define (toString rat)
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



