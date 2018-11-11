#lang racket

(define (average a b)
  (/ (+ a b) 2))

(define (close-enough? a b)
  (< (abs (- a b)) 0.0001))

(define (search f neg pos)
  (let ((mid (average neg pos)))
    (if (close-enough? neg pos)
        mid
        (let ((test-value (f mid)))
          (cond ((positive? test-value)
                 (search f neg mid))
                ((negative? test-value)
                 (search f mid pos))
                (else mid))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (positive? a-value) (negative? b-value))
           (search f b a))
          (else
           (error "Arguments has same sign")))))
