#lang sicp

(define (last-pair l)
  (if (null? (cdr l))
      (car l)
      (last-pair (cdr l))))

(define (reverse lst)
    (iter lst '()))

(define (iter remained-items result)
    (if (null? remained-items)
        result
        (iter (cdr remained-items)
              (cons (car remained-items) result))))