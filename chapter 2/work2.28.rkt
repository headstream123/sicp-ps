#lang sicp

(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else
         (append (fringe (car tree))
                 (fringe (cdr tree))))))