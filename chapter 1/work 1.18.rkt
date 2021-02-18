#lang sicp

(define (double a) (+ a a))
(define (halve a) (/ a 2))
(define (even? a) (= (remainder a 2) 0))

(define (fast-mul a b)
  (define (iter a b f)
    (cond ((= b 0) f)
          ((even? b) (iter (double a) (halve b) f))
          (else (iter a (- b 1) (+ f a)))))
  (iter a b 0))
  