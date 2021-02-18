#lang sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (define (iter x a)
    (if (= 1 (remainder x 2))
        a
        (iter (/ x 2) (+ a 1))))
  (iter x 0))

(define (cdr x)
  (define (iter x a)
    (if (= 1 (remainder x 3))
        a
        (iter (/ x 3) (+ a 1))))
  (iter x 0))