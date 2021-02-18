#lang sicp

(define (cubt-iter guess x)
  (if (good-enough? guess (improve guess x))
      (improve guess x)
      (cubt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/(+ (* 2 guess) (/ x (square guess))) 3))

(define (good-enough? ex-guess guess)
  (< (/ (abs(- guess ex-guess)) ex-guess) 0.001))

(define (square x) (* x x))

(define (cubt x)
  (cubt-iter 1.0 x))
