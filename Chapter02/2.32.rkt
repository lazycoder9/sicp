#lang racket

(define (subsets s) 
  (if (null? s) 
      '() 
      (let ((rest (subsets (cdr s)))) 
        (append rest (map (lambda (ss) (cons (car s) ss)) rest)))))

(define x (list 1 2 3))













  