#lang sicp

(define (split op1 op2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (op2 painter (op1 smaller smaller)))))
  iter)

(define up-split (split beside below))
(define right-split (split below beside))
