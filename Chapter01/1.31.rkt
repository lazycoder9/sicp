#lang racket

(require rackunit)

; A
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (inc x) (+ 1 x))

(define (identity x) x)

(define (factorial x)
  (product identity 1 inc x))

(define (pi-4 n)
  (define (term k)
    (define c (quotient k 2))
    (/ (+ 2 (* 2.0 c))
       (+ 3 (* 2 (if (even? k) (- c 1) c)))))
  (product term 1 inc n))

; TEST
(check-eq? (factorial 5) 120)
(check-eq? (factorial 0) 1)

; B

(define (product-iter term a next b)
  (define (iter n result)
    (if (> n b)
        result
        (iter (next n) (* result (term n)))))
  (iter a 1))

(define (factorial2 x)
  (product-iter identity 1 inc x))

; TEST B
(check-eq? (factorial2 5) 120)
(check-eq? (factorial 0) 1)

