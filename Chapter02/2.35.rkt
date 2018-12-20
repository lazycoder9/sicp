#lang racket
(require rackunit)
(require "2.33.rkt")

(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (if (pair? x)
                                   (count-leaves x)
                                   1)) t)))

;TEST
(define tree (list 1 (list 2 3 4) 5 (list 6 7)))
(check-equal? (count-leaves tree) 7)
