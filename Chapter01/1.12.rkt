#lang racket

(require rackunit)

(define (dec x) (- x 1))

(define (inc x) (+ x 1))

(define (pascal-triangle row n)
  (cond ((= n 0) 1)
        ((= n row) 1)
        (else (+ (pascal-triangle (dec row) (dec n))
                 (pascal-triangle (dec row) n)))))

;TEST

(check-eq? (pascal-triangle 4 0) 1)
(check-eq? (pascal-triangle 4 1) 4)
(check-eq? (pascal-triangle 4 2) 6)
(check-eq? (pascal-triangle 4 3) 4)
(check-eq? (pascal-triangle 4 4) 1)