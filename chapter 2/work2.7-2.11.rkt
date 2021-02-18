#lang sicp

(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

(define (sub-interval x y)
  (add-interval x
                (make-interval (-(upper-bound y))
                               (-(lower-bound y)))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (define (opposite-pair? a b)
    (if (positive? a)
        (negative? b)
        (positive? b))) 
  
  (define (positive-pair? a b)
    (if (opposite-pair? a b)
        #f
        (positive? a))) 
  
  (define (negative-pair? a b)
    (if (opposite-pair? a b)
        #f
        (negative? a)))
  (let ((x0 (lower-bound x))
        (x1 (upper-bound x))
        (y0 (lower-bound y))
        (y1 (upper-bound y)))
    (cond ((positive-pair? x0 x1)
           (cond ((positive-pair? y0 y1)
                  (make-interval (* x0 y0) (* x1 y1)))
                 ((negative-pair? y0 y1)
                  (make-interval (* x1 y0) (* x0 y1)))
                 (else
                  (make-interval (* x1 y0) (* x1 y1)))))
          ((negative-pair? x0 x1)
           (cond ((positive-pair? y0 y1)
                  (make-interval (* x0 y1) (* x1 y0)))
                 ((negative-pair? y0 y1)
                  (make-interval (* x1 y1) (* x0 y0)))
                 (else
                  (make-interval (* x0 y1) (* x0 y0)))))
          (else
           (cond ((positive-pair? y0 y1)
                  (make-interval (* x0 y1) (* x1 y1)))
                 ((negative-pair? y0 y1)
                  (make-interval (* x1 y0) (* x0 y0)))
                 (else
                  (make-interval
                   (min (* x0 y1) (* x1 y0))
                   (max (* x0 y0) (* x1 y1))))))))) 
 

(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (not (> (* (lower-bound y) (upper-bound y)) 0))
      (error "Div 0:" y)
      (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))