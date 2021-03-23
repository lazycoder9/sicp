#lang racket
(require rackunit)

; VECTOR ABSTRACTIONS
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

; FRAME ABSTRACTIONS

; ABSTRACTION WITH LIST
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (first frame))

(define (edge1-frame frame)
  (second frame))

(define (edge2-frame frame)
  (third frame))

;ABSTRACTION WITH PAIRS
(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (cadr frame))

(define (edge2-frame2 frame)
  (cddr frame))

(define origin (cons 1 2))
(define edge1 (make-vect 2 3))
(define edge2 (make-vect 3 4))

(define list-frame (make-frame origin edge1 edge2))
(define pair-frame (make-frame2 origin edge1 edge2))

(check-equal? (origin-frame list-frame) (origin-frame2 pair-frame))
(check-equal? (edge1-frame list-frame) (edge1-frame2 pair-frame))
(check-equal? (edge2-frame list-frame) (edge2-frame2 pair-frame))
