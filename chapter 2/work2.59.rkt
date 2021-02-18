#lang sicp

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1)
                 (adjoin-set (car set1) set2))))

(define (union-set1 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) '())
        ((element-of-set? (car set1) set2)
         (union-set1 (cdr set1) set2))
        (else
         (cons (car set1)
               (union-set1 (cdr set1) set2)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))