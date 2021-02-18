#lang sicp

(define (fast-mul a b)
  (define (double a) (+ a a))
  (define (halve a) (/ a 2))
  (define (even? a) (= (remainder a 2) 0))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul a (halve b))))
        (else (+ a (fast-mul a (- b 1))))))