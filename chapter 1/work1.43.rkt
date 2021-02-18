#lang sicp

(define (repeated f n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (+ i 1) (compose f result))))
  (iter 1 f))

(define (repeated1 f n)
  (if (= n 1)
      f
      (compose f (repeated1 f (- n 1)))))

(define (compose f g)
  (lambda (x)
    (f (g x))))    

(define (square x) (* x x))