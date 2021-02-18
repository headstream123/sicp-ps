#lang sicp

(define (for-each f lst)
  (if (null? lst)
      (newline)
      (cons (f (car lst)) (for-each f (cdr lst)))))