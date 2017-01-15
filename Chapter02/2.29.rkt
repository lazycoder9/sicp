#lang racket

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