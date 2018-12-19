#lang racket
(require rackunit)

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
        (total-weight struct)
        struct)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (define (branch-balanced? branch)
    (if (pair? (branch-structure branch))
        (balanced? (branch-structure branch))
        true))
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (torque left) (torque right)))))

;TEST
(define b-1 (make-branch 2 5))
(define b-2 (make-branch 1 4))

;1st mobile
;      |
;|```````````|
;2           2

(define m-1 (make-mobile b-1 b-1))
(check-true (balanced? m-1))

;2st mobile
;      |
;|``````````|
;2          1
(define m-2 (make-mobile b-1 b-2))
(check-false (balanced? m-2))
