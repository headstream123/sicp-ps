#lang sicp

(define f
  (let ((n 1))
    (lambda (x)
      (set! n (* x n)) n)))