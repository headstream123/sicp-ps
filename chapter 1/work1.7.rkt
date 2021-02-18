#lang sicp

(define (sqrt-iter ex-guess guess x)
  (if (good-enough? ex-guess guess)
      guess
      (sqrt-iter guess (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? ex-guess guess)
  (< (/ (abs (- guess ex-guess)) ex-guess) 0.001))

(define (sqrt x)
  (sqrt-iter 0.0 1.0 x))
