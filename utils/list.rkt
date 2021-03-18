#lang racket

(provide (all-defined-out))

(define (accumulate op init items)
  (if (null? items)
      init
      (op (car items)
          (accumulate op init (cdr items)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(define (permutation s)
  (if (null? s)
      '()
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutation (remove x s))))
               s)))
