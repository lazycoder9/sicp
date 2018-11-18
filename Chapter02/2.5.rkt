#lang racket

(require rackunit)

(define (pow a b)
  (round (exp (* b (log a)))))

(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (car pair)
  (define (iter pair count)
    (if (= (remainder pair 2) 0)
        (iter (/ pair 2) (+ count 1))
        count))
  (iter pair 0))

(define (cdr pair)
  (define (iter pair count)
    (if (= (remainder pair 3) 0)
        (iter (/ pair 3) (+ count 1))
        count))
  (iter pair 0))

;TEST
(define x (cons 2 5))

(check-eq? (car x) 2)
(check-eq? (cdr x) 5)
