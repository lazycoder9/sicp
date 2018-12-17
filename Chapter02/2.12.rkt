#lang racket

(require rackunit)
(require "2.7-2.8.rkt")
(provide make-center-percent)

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((percent (/ p 100.0)))
        (make-interval (* c (- 1 percent))
                       (* c (+ 1 percent)))))

(define (percent i)
  (* (/ (width i) (center i)) 100))

;TEST
(define i (make-center-percent 10 10))

(check-equal? (lower-bound i) 9.0)
(check-equal? (upper-bound i) 11.0)
