#lang sicp

(define (cont-frac n d k)
  (define (rec n d i k)
     (if (> i k)
      0
      (/ (n i) (+ (d i) (rec n d (+ i 1) k)))))
  (rec n d 1 k))

(define (cont-frac1 n d k)
  (define (iter i result)
    (if (< i 1)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))