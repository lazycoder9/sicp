#lang racket

(require rackunit)

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product a1 a2)
  (cond ((or (=number? a1 0) (=number? a2 0)) 0)
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (let ((rest (cddr s)))
    (if (and (pair? rest) (> (length rest) 1))
        (cons '+ rest)
        (car rest))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (let ((rest (cddr p)))
        (if (and (pair? rest) (> (length rest) 1))
            (cons '* rest)
            (car rest))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (- (exponent exp) 1)))
          (deriv (base exp) var)))
        (else
         (error "Unknown expression -- DERIV" exp))))

; ax^2 => 2ax
(check-equal? (deriv '(* a (** x 2)) 'x) '(* a (* 2 x)))
; ax^2 + bx + c => 2ax + b
(check-equal? (deriv '(+ (* a (** x 2)) (* b x) c) 'x) '(+ (* a (* 2 x)) b))
; xy(x + 3) => xy + y(x + 3)
(check-equal? (deriv '(* x y (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))
