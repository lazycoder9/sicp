#lang racket

(define (accumulate op init items)
  (if (null? items)
      init
      (op (car items)
          (accumulate op init (cdr items)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low
            (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

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

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k position)
  (define (queens-safe? queen-count rest-rows)
    (define (queen-safe? col row)
      (let ((last-col k) (last-row (car position)))
        (and (not (= last-row row))
             (not (= (abs (- last-row row))
                     (abs (- last-col col)))))))
    (cond ((null? rest-rows) true)
          ((queen-safe? queen-count (car rest-rows))
             (queens-safe? (- queen-count 1) (cdr rest-rows)))
          (else false)))
  (queens-safe? (- k 1) (cdr position)))