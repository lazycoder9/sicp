#lang racket
(require rackunit)

(require (only-in "../utils/list.rkt"
                  accumulate))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (accumulate + 0 (map * v x))) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x)) m)))

(define m (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define v (list 5 6 7))

(check-equal? (dot-product v (car m)) 38)
(check-equal? (dot-product (car m) (cadr m)) 32)

(check-equal? (matrix-*-vector m v) '(38 92 146))

(check-equal? (transpose m) '((1 4 7) (2 5 8) (3 6 9)))

(check-equal? (matrix-*-matrix m m) '((30 36 42) (66 81 96) (102 126 150)))
(check-equal? (matrix-*-matrix m (transpose m)) '((14 32 50) (32 77 122) (50 122 194)))
