#lang sicp

(define (iterative-improve good? improve)
  (lambda (x)
    (let ((f (iterative-improve good? improve)))
      (if (good? x)
        x
        (f (improve x))))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve close-enough? improve) first-guess))



(define (squrt-iter x guess)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) guess))

(define (squrt x)
  (squrt-iter x 1.0))

(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))