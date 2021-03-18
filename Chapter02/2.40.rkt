#lang racket

(require (only-in "../utils/list.rkt"
                  accumulate))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))

(define (square x) (* x x))

(define (smallest-divisor n)
  (define (iter divisor)
    (cond ((> (square divisor) n) 1)
          ((= (remainder n divisor) 0) divisor)
          (else (iter (+ divisor 1)))))
  (iter 2))

(define (prime? n)
  (= (smallest-divisor n) 1))

(define n 6)

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(define (permutation s)
  (if (null? s)
      '()
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutation (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(define (unique-pairs n)
  (flatmap (lambda (i) (map (lambda (j) (list i j))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triples n)
  (flatmap (lambda (i) (flatmap (lambda (j)
                            (map (lambda (k) (list i j k))
                                 (enumerate-interval 1 (- j 1))))
                            (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sum s n)
  (filter (lambda (x) (= (accumulate + 0 x) s)) (triples n)))
