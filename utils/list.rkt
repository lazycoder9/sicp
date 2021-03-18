#lang racket
(provide accumulate)

(define (accumulate op init items)
  (if (null? items)
      init
      (op (car items)
          (accumulate op init (cdr items)))))

