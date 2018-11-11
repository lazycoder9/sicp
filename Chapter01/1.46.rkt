#lang racket

(define (iterative-improve check improve)
  (lambda (x) (if (check x)
                  x
                  (check (improve x)))))
