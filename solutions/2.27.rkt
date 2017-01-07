#lang racket

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
    (cond ((null? list)
           rev-list)
          ((pair? (car list))
           (iter (cons (iter '() (car list)) rev-list) (cdr list)))
          (else
           (iter (cons (car list) rev-list) (cdr list)))))
  (iter '() l))

(define x (list (list 1 2) (list 3 4)))








  