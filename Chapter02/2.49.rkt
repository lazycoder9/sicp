#lang sicp
(#%require 2htdp/image)
(#%require lang/posn)
(#%require racket/base)

(define *current-image* empty-image)

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


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (*new-image* new-frame)
  (define (xy->posn x y)
    (let ((v ((frame-coord-map new-frame) (make-vect x y))))
      (make-posn (xcor-vect v) (ycor-vect v))))
  (set! *current-image*
        (polygon
         (list
          (xy->posn 0 0)
          (xy->posn 0 1)
          (xy->posn 1 1)
          (xy->posn 1 0))
         "solid"
         "white")))  

(define (draw-line start end)
    (set! *current-image*
        (add-line
         *current-image*
         (xcor-vect start)
         (ycor-vect start)
         (xcor-vect end)
         (ycor-vect end)
         "black")))  

(define (paint-in-frame painter frame)
    (*new-image* frame)
    (painter frame)
    *current-image*)  

(define (paint painter)
    (paint-in-frame
        painter
        (make-frame
            (make-vect 0 150)
            (make-vect 150 0)
            (make-vect 0 -150))))

(define (make-segment start end)
  (cons start end))


(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (segments->painter segment-list)   
 (lambda (frame)     
  (for-each     
   (lambda (segment)        
    (draw-line         
     ((frame-coord-map frame) (start-segment segment))         
     ((frame-coord-map frame) (end-segment segment))))      
 segment-list)))

(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.20 0.00) (make-vect 0.35 0.50))
    (make-segment (make-vect 0.35 0.50) (make-vect 0.30 0.60))
    (make-segment (make-vect 0.30 0.60) (make-vect 0.15 0.45))
    (make-segment (make-vect 0.15 0.45) (make-vect 0.00 0.60))
    (make-segment (make-vect 0.00 0.80) (make-vect 0.15 0.65))
    (make-segment (make-vect 0.15 0.65) (make-vect 0.30 0.70))
    (make-segment (make-vect 0.30 0.70) (make-vect 0.40 0.70))
    (make-segment (make-vect 0.40 0.70) (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.85) (make-vect 0.40 1.00))
    (make-segment (make-vect 0.60 1.00) (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85) (make-vect 0.60 0.70))
    (make-segment (make-vect 0.60 0.70) (make-vect 0.75 0.70))
    (make-segment (make-vect 0.75 0.70) (make-vect 1.00 0.40))
    (make-segment (make-vect 1.00 0.20) (make-vect 0.60 0.48))
    (make-segment (make-vect 0.60 0.48) (make-vect 0.80 0.00))
    (make-segment (make-vect 0.40 0.00) (make-vect 0.50 0.30))
    (make-segment (make-vect 0.50 0.30) (make-vect 0.60 0.00)))))

(define square-frame
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 0.0))
    (make-segment (make-vect 1.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 1.0 1.0) (make-vect 0.0 1.0))
    (make-segment (make-vect 0.0 1.0) (make-vect 0.0 0.0)))))

(define x-frame
  (segments->painter
   (list
    (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
    (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))

(define diamond-frame
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
    (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
    (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5))
    (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0)))))

(paint diamond-frame)
