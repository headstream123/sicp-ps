#lang sicp

(define (same-parity x . lst)
  (define (parity? a b)
    (= (remainder a 2) (remainder b 2)))
  (define (f x lst)
    (cond ((null? lst) (list x))
        ((parity? x (car lst))
         (cons x (f (car lst) (cdr lst))))
        (else
         (f x (cdr lst)))))
  (f x lst))

