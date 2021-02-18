#lang sicp

(define (make-rectangle long short)
  (cons long short))

(define (long-side rec) (car rec))
(define (short-side rec) (cdr rec))

(define (rec-circum rec)
  (let ((l (long-side rec))
        (s (short-side rec)))
    (* 2 (+ (length l) (length s)))))

(define (rec-area rec)
  (let ((l (long-side rec))
        (s (short-side rec)))
    (* (length l) (length s))))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment sp ep) (cons sp ep))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (length s)
  (let ((start (start-segment s))
        (end (end-segment s)))
    (sqrt (+ (square (- (x-point start) (x-point end)))
              (square (- (y-point start) (y-point end)))))))

(define (square x) (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))