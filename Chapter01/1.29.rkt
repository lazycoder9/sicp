#lang racket

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (coef k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (* (coef k) (f (+ a (* k h)))))
  (* (/ h 3) (sum term 0 inc n)))