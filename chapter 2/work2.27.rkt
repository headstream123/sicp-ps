#lang sicp

(define (reverse lst)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l)
              (cons (car l) result))))
  (iter lst nil))

(define (deep-reverse lst)
  (define (iter l result)
    (cond ((null? l) result)
          ((not (pair? l)) l)
          (else
           (iter (cdr l)
                 (cons (iter (car l) nil)
                       result)))))
  (iter lst nil))