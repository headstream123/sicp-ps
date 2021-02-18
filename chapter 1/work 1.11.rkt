#lang sicp

(define (p-rec n)
  (if (< n 3)
      n
      (+ (p-rec (- n 1))
         (* 2 (p-rec (- n 2)))
         (* 3 (p-rec (- n 3))))))

(define (p max)
  (define (p-iter a b c counter)
    (if (> counter max)
        a
        (p-iter b
                c
                (+ c (* 2 b) (* 3 a))
                (+ counter 1))))
  (p-iter 0 1 2 1))