#lang racket

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

(define (for-each proc items)
  (cond ((null? items)
         '())
        (else
         (proc (car items))
         (for-each proc (cdr items)))))
      

(square-list-map (list 1 2 3 4 5))













  