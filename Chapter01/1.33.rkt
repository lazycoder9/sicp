#lang racket

(require rackunit)

(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter n result)
    (cond ((> n b) result)
          ((predicate n) (iter (next n) (combiner (term n) result)))
          (else (iter (next n) result))))
  (iter a null-value))

(define (inc x) (+ x 1))

(define (identity x) x)

(define (square x) (* x x))

(define (smallest-divisor n)
  (define (iter d)
    (cond ((> (square d) n) 1)
          ((= (remainder n d) 0) d)
          (else (iter (inc d)))))
  (iter 2))

(define (prime? n)
  (= (smallest-divisor n) 1))

(define (sum-of-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-of-pair-primes n)
  (define (pair-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc (- n 1) pair-prime?))

; TEST
(check-eq? (sum-of-prime-squares 3 6) 34)
(check-eq? (product-of-pair-primes 10) 189)

