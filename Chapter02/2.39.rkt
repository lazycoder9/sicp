#lang racket

(define (fold-right op init items)
  (if (null? items)
      init
      (op (car items)
          (fold-right op init (cdr items)))))

(define (fold-left op init items)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init items))

(define (reverse-right items)
  (fold-right (lambda (x y) (append y (list x))) '() items))

(define (reverse-left items)
  (fold-left (lambda (x y) (cons y x)) '() items))

(define x (list 1 2 3))