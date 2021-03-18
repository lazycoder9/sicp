#lang racket

(require (only-in "../utils/list.rkt"
                  flatmap
                  enumerate-interval
                  accumulate))

(define (triples n)
  (flatmap (lambda (i) (flatmap (lambda (j)
                            (map (lambda (k) (list i j k))
                                 (enumerate-interval 1 (- j 1))))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sum s n)
  (filter (lambda (x) (= (accumulate + 0 x) s)) (triples n)))
