#lang sicp

(define (simpson f a b n)
  (define (k x)
    (/ (- x a) (/ (- b a) n)))
  (define (next x)
    (+ x (/ (- b a) n)))
  (define (sum term a next b)
    (cond((> a b) 0)
       ((or (= 0 (k a)) (= b a))
        (+ (term a)
           (sum term (next a) next b)))
       ((even? (k a))
        (+ (* (term a) 2)
           (sum term (next a) next b)))
       (else
        (+ (* (term a) 4)
           (sum term (next a) next b)))))
  (* (sum f a next b) (/ (- b a) (* 3.0 n))))

(define (cube x) (* x x x))