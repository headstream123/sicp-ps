#lang sicp

(define (n-roots x n)
  (define f
    (lambda (y)
      (/ x (exp y (- n 1)))))
  (fixed-point
   ((repeated average-damp (log2 n)) f)
   1.0))

(define (log2 n)
  (if (< n 4)
      1
      (floor (/ (log n) (log 2)))))

(define (exp base n)
  (cond ((= n 0) 1)
        ((even? n) (square (exp base (/ n 2))))
        (else (* base (exp base (- n 1))))))

(define (square x) (* x x))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y) (/ (+ x y) 2))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define (compose f g)
  (lambda (x)
    (f (g x))))