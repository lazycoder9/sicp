#lang racket

(define (cont-frac n d k) 
    (define (iter i) 
      (/ (n i) (+ (d i) 
                  (if (< i k) 
                      (iter (+ i 1)) 
                      0)))) 
    (iter 1)) 

(define (eiler k)
  (if (= (remainder k 3) 2)
      (* 2 (/ (+ k 1) 3))
      1))

(define (e k)
  (+ 2 (cont-frac (lambda (i) 1.0) (lambda (i) (eiler i)) k)))

(define (tan-cf x k)
  (cont-frac (lambda (i) (if (= i 1)
                             x
                             (- (* x x))))
             (lambda (i) (- (* i 2) 1))
             k))

(define (square x)
  (* x x))

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
          (if (close-enough? guess next)
              next
              (try next))))
  (try first-guess))

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube x) (* x x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (iterative-improve check improve)
  (lambda (x) (if (check x)
                  x
                  (check (improve x)))))



             
  



