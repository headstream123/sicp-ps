#lang sicp

(define dx 0.00001)

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx))) 3)))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (smooth-n-times f n)
  (let ((fs (repeated smooth n)))
    (fs f)))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x) (* x x))