#lang sicp

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define (make-center-percent c p)
  (make-interval (* c (- 1 p))
                 (* c (+ 1 p))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (percent i)
  (/ (width i) (center i)))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

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
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (cond ((not (> (* (lower-bound y) (upper-bound y)) 0))
         (error "Div 0:" y))
        ((and (= (lower-bound x) (lower-bound y))
              (= (upper-bound x) (upper-bound y)))
         (make-interval 1 1))
        (else
         (mul-interval x
                       (make-interval (/ 1.0 (upper-bound y))
                                      (/ 1.0 (lower-bound y)))))))