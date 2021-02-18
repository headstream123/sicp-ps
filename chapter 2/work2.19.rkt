#lang sicp

(define (no-more? lst)
  (if (null? lst)
      #t
      #f))

(define (except-first-denomination lst)
  (cdr lst))

(define (first-denomination lst)
  (car lst))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))