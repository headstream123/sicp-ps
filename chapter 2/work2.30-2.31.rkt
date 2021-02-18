#lang sicp

(define (tree-map f t)
  (cond ((null? t) nil)
        ((not (pair? t)) (f t))
        (else
         (cons (tree-map f (car t))
               (tree-map f (cdr t))))))

(define (square-tree1 t)
  (map (lambda (x)
         (if (pair? x)
             (square-tree1 x)
             (square x)))
       t))

(define (square-tree t)
  (cond ((null? t) nil)
        ((not (pair? t)) (square t))
        (else
         (cons (square-tree (car t))
               (square-tree (cdr t))))))

(define (square z) (* z z))