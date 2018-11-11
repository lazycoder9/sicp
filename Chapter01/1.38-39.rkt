#lang racket

(define (cont-frac n d k)
    (define (iter i)
      (/ (n i) (+ (d i)
                  (if (< i k)
                      (iter (+ i 1))
                      0))))
    (iter 1))

(define (eiler k)
  (if (= (remainder k 3) 2)
      (* 2 (/ (+ k 1) 3))
      1))

(define (e k)
  (+ 2 (cont-frac (lambda (i) 1.0) (lambda (i) (eiler i)) k)))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))

