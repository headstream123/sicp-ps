#lang sicp

(define (equal? x y)
  (cond ((and (number? x) (number? y))
         (= x y))
        ((and (symbol? x) (symbol? y))
         (eq? x y))
        ((and (pair? x) (pair? y)
              (equal? (car x) (car y)))
         (equal? (cdr x) (cdr y)))
        (else #f)))