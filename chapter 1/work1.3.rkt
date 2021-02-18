#lang sicp
(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)))
(define (smaller x y)
  (if (< x y)
      x
      y))
(define (bigger x y)
  (if (< x y)
      y
      x))
(define (sum-bigger x y z)
  (sum-squares (bigger x y) (bigger (smaller x y) z)))
(define (sb x y z)
  (cond ((and (< x y) (< x z)) (sum-squares y z))
        ((and (< y x) (< y z)) (sum-squares x z))
        ((and (< z x) (< z y)) (sum-squares x y))))