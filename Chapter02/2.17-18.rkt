#lang racket
(require rackunit)

(provide reverse)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (last-pair list)
  (if (= (length list) 1)
      list
      (last-pair (cdr list))))

(define (reverse l)
  (define (iter rev-list list)
    (if (null? list)
        rev-list
        (iter (cons (car list) rev-list) (cdr list))))
  (iter (list ) l))

;TEST
(define a (list 23 72 149 34))
(check-equal? (list 34) (last-pair a))
(check-equal? (list 34 149 72 23) (reverse a))
