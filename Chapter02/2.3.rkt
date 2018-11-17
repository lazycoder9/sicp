#lang racket

(require rackunit)
(require "2.2.rkt")

(define (make-rectangle upper-left-point h w)
  (cons (cons h w) upper-left-point))

(define (height rec)
  (caar rec))

(define (width rec)
  (cdar rec))

(define (upper-left-point rec)
  (cdr rec))

(define (perimeter rec)
  (* 2 (+ (width rec) (height rec))))

(define (area rec)
  (* (width rec) (height rec)))

;TEST
(define rec (make-rectangle (make-point 0 0) 2 4))

(check-eq? (perimeter rec) 12)
(check-eq? (area rec) 8)
