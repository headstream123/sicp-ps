#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (ident x) x)
  (define (inc x) (+ x 1))
  (product ident 1 inc n))

(define (pi-product n)
  (define (pi-term x)
    (/ (* (- x 1) (+ x 1))
       (* x x)))
  (define (pi-next x) (+ x 2))
  (* (product1 pi-term 3.0 pi-next n) 4))

(define (product1 term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))