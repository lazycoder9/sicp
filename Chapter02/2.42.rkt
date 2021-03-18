#lang racket

(require rackunit)
(require (only-in "../utils/list.rkt"
                  enumerate-interval
                  flatmap))

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

(define (safe? k positions)
  (define (queens-safe? queens-count rest-queens)
    (define (queen-safe? col row)
      (let ((last-col k)
            (last-row (car positions)))
        (and (not (= last-row row))
             (not (= (abs (- last-col col))
                     (abs (- last-row row)))))))
    (cond ((null? rest-queens) #true)
          ((queen-safe? queens-count (car rest-queens))
             (queens-safe? (- queens-count 1) (cdr rest-queens)))
          (else #false)))
  (queens-safe? (- k 1) (cdr positions)))

(check-equal? (queens 4) '((3 1 4 2) (2 4 1 3)))


