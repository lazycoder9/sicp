#lang racket
(define (reverse l)
  (define (iter rev-list list)
    (if (null? list)
        rev-list
        (iter (cons (car list) rev-list) (cdr list))))
  (iter (list ) l))

(define (same-parity x . n)
  (let ((parity (remainder x 2)))
    (define (iter result oldlist)
      (cond ((= (length oldlist) 0)
             (reverse result))
            ((= (remainder (car oldlist) 2) parity)
             (iter (cons (car oldlist) result) (cdr oldlist)))
            (else
             (iter result (cdr oldlist)))))
    (iter (list x) n)))

(same-parity 2 3 4 5 6)













  