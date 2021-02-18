#lang sicp

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init
                        (map car seqs))
            (accumulate-n op init
                          (map cdr seqs)))))

(define (count-leaves t)
  (accumulate
   +
   0
   (map
    (lambda (x)
      (if (not (pair? x))
          1
          (count-leaves x)))
    t)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))