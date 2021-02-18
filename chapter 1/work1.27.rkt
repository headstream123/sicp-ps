#lang sicp

(define (carmicheal-test n)
  (define (carm-iter a n)
    (cond ((= a n) #t)
          ((try-it a n) (carm-iter (+ a 1) n))
          (else #f)))
  (carm-iter 1 n))

(define (try-it a n)
  (= (expmod a n n) (remainder a n)))
  
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        ( else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (square n) (* n n))