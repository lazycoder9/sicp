#lang racket
(require rackunit)
(require (only-in "../utils/list.rkt"
                  accumulate))

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;TEST
(define x (list 1 2 3 4))
(define y (list 5 6 7))

(check-equal? (map (lambda (x) (* x 2)) x) '(2 4 6 8))
(check-equal? (append x y) '(1 2 3 4 5 6 7))
(check-equal? (length x) 4)
