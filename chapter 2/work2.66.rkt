#lang sicp

(define (lookup k records)
  (cond ((null? records) #f)
        ((= k (key (entry records)))
         (entry records))
        ((> k (key (entry records)))
         (lookup k (right-branch records)))
        ((< k (key (entry records)))
         (lookup k (left-branch records)))))


(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))