#lang sicp

(define (bal-mobile? m)
  (define (bal m)
    (cond ((not (or (pair? (branch-length m))
                    (pair? (branch-structure m))))
           (* (branch-structure m)
              (branch-length m)))
          ((= (bal (left-branch m))
              (bal (right-branch m)))
           (* 2 (bal (right-branch m))))
          (else
           0)))
  (if (= (bal m) 0)
      #f
      #t))

(define (total-weight m)
  (cond ((not (or (pair? (branch-length m))
                  (pair? (branch-structure m))))
         (branch-structure m))
        (else
         (+ (total-weight (left-branch m))
            (total-weight (right-branch m))))))

(define (total-weight-low m)
  (define (half-weight b)
    (if (not (pair? b))
        b
        (half-weight (branch-structure b))))
  (+ (half-weight (left-branch m))
     (half-weight (right-branch m))))

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m) (car m))
(define (right-branch m) (cadr m))

(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))

