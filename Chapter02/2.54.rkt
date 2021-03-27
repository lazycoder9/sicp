#lang racket

(require rackunit)

(define (equal? list1 list2)
  (cond ((not (= (length list1) (length list2))) #false)
        ((and (null? list1) (null? list2)) #true)
        (else
         (let ((first1 (car list1))
               (first2 (car list2)))
           (cond ((and (symbol? first1) (symbol? first2) (eq? first1 first2)) (equal? (cdr list1) (cdr list2)))
                 ((and (list? first1) (list? first2) (equal? first1 first2)) (equal? (cdr list1) (cdr list2)))
                 (else #false))))))

(check-equal? (equal? '(a) '(a))  #true)
(check-equal? (equal? '(a b c) '(a b c))  #true)
(check-equal? (equal? '(a) '((a)))  #false)
(check-equal? (equal? '(this is a list) '(this (is a) list))  #false)
