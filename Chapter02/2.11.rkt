#lang racket

(define (mul-interval x y)
  (let ((xl (lower-bound x))
        (xu (upper-bound x))
        (yl (lower-bound y))
        (yu (upper-bound y)))
    (cond ((>= xl 0)
           (cond ((>= yl 0)
                  (make-interval (* xl yl) (* xu yu)))
                 ((<= yu 0)
                  (make-interval (* xu yl) (* xl yu)))
                 (else
                  (make-interval (* xu yl) (* xu yu)))))
          ((and (<= xu 0) ( xu 0) (< xl 0))
           (cond (( yu 0) (< yl 0))
                  (make-interval (min (* xl yu) (* xu yl))
                                 (max (* xl yl) (* xu yu))))))
          (else (mul-interval y x))))
