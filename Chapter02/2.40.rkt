#lang racket

(require (only-in "../utils/list.rkt"
                  accumulate
                  enumerate-interval
                  remove
                  flatmap))

(require (only-in "../utils/misc.rkt"
                  square
                  smallest-divisor
                  prime?))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
