#lang racket
(require rackunit)

(define (accumulate op init items)
  (if (null? items)
      init
      (op (car items)
          (accumulate op init (cdr items)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;TEST
(define x (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(check-equal? (accumulate-n + 0 x) (list 22 26 30))
