#lang racket

(require rackunit)
(provide make-interval)
(provide lower-bound)
(provide upper-bound)
(provide add-interval)
(provide mul-interval)
(provide div-interval)
(provide sub-interval)

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;TEST
(define x (make-interval 4 6))
(define y (make-interval 2 4))

(define sum (add-interval x y))

(check-eq? (lower-bound sum) 6)
(check-eq? (upper-bound sum) 10)

(define mul (mul-interval x y))

(check-eq? (lower-bound mul) 8)
(check-eq? (upper-bound mul) 24)

(define div (div-interval x y))

(check-equal? (lower-bound div) 1.0)
(check-equal? (upper-bound div) 3.0)

(define sub (sub-interval x y))

(check-eq? (lower-bound sub) 0)
(check-eq? (upper-bound sub) 4)
