#lang sicp
(#%require sicp-pict)
;(paint f)

(define wave
  (let ((a (make-vect 0.25 0))
        (b (make-vect 0.3 0.5))
        (c (make-vect 0.28 0.55))
        (d (make-vect 0.18 0.4))
        (e (make-vect 0 0.6))
        (f (make-vect 0 0.8))
        (g (make-vect 0.18 0.55))
        (h (make-vect 0.28 0.6))
        (i (make-vect 0.4 0.6))
        (j (make-vect 0.32 0.8))
        (k (make-vect 0.4 1))
        (l (make-vect 0.6 1))
        (m (make-vect 0.68 0.8))
        (n (make-vect 0.6 0.6))
        (o (make-vect 0.63 0.6))
        (p (make-vect 1 0.4))
        (q (make-vect 1 0.2))
        (r (make-vect 0.6 0.48))
        (s (make-vect 0.75 0))
        (t (make-vect 0.6 0))
        (u (make-vect 0.5 0.32))
        (v (make-vect 0.4 0))
        (w (make-vect 0.4 0.75))
        (x (make-vect 0.45 0.7))
        (y (make-vect 0.55 0.7))
        (z (make-vect 0.6 0.75)))
    (segments->painter
     (list
      (make-segment a b) (make-segment b c)
      (make-segment c d) (make-segment d e)
      (make-segment f g) (make-segment g h)
      (make-segment h i) (make-segment i j)
      (make-segment j k) (make-segment l m)
      (make-segment m n) (make-segment n o)
      (make-segment o p) (make-segment q r)
      (make-segment r s) (make-segment t u)
      (make-segment u v) (make-segment w x)
      (make-segment x y) (make-segment y z)))))

(define painter3
  (let ((or (make-vect 0.5 0))
        (ot (make-vect 0 0.5))
        (ttr (make-vect 0.5 1))
        (rtr (make-vect 1 0.5)))
    (segments->painter
     (list
      (make-segment ot ttr) (make-segment ttr rtr)
      (make-segment rtr or) (make-segment or ot)))))

(define painter2
  (segments->painter
   (list
    (make-segment o tr) (make-segment t r))))

(define painter1
  (segments->painter
   (list
    (make-segment o r) (make-segment r tr)
    (make-segment tr t) (make-segment t o))))

(define o (make-vect 0 0))
(define t (make-vect 1 0))
(define r (make-vect 0 1))
(define tr (make-vect 1 1))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (make-segment start end)
  (list start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f)
  (car f))

(define (edge1-frame f)
  (cadr f))

(define (edge2-frame f)
  (caddr f))

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
