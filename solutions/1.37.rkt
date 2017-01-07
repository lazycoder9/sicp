#lang racket

(define (cont-frac-rec n d k)
 (if (= k 0)
     d
     (/ n (+ d (cont-frac-rec n d (- k 1))))))

(define (cont-frac-iter n d k)
  (define (iter k div)
    (if (= k 0)
        div
        (iter (- k 1) (/ n (+ d div)))))
  (iter k (/ n d)))

