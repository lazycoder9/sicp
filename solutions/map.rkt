#lang racket

(define (scale-list items factor)
  (if (null? items)
      '()
      (cons (* (car items) factor)
        (scale-list (cdr items) factor))))


(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(map (lambda (x) (* x 2)) (list 1 2 3 4 5))













  