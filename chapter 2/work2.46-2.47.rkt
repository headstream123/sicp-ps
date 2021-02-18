#lang sicp
(#%require sicp-pict)
;(paint f)

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame1 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

(define (edge2-frame1 f)
  (cddr f))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

(define (sub-vect v w)
  (let ((vx (xcor-vect v))
        (vy (ycor-vect v))
        (wx (xcor-vect w))
        (wy (ycor-vect w)))
    (make-vect (- vx wx) (- vy wy))))

(define (add-vect v w)
  (let ((vx (xcor-vect v))
        (vy (ycor-vect v))
        (wx (xcor-vect w))
        (wy (ycor-vect w)))
    (make-vect (+ vx wx) (+ vy wy))))

(define (make-vect x y)
  (list x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cadr v))
