#lang sicp

(define (fast-expt b n)
  (define (even? x)
    (= (remainder x 2) 0))
  (define (iter b counter a)
    (cond ((= counter 0) a)
          ((even? counter)
           (iter (* b b) (/ counter 2) a))
          (else (iter b (- counter 1) (* a b)))))
  (iter b n 1))