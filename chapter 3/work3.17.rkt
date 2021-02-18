#lang sicp

(define count-pairs
  (let ((path '()))
    (lambda (x)
      (if (not (pair? x))
          0
          (if (in-path? x path)
              0
              (begin (set! path (cons x path))
                     (+ (count-pairs (car x))
                        (count-pairs (cdr x))
                        1)))))))

(define (in-path? x path)
  (cond ((null? path) #f)
        ((eq? x (car path)) #t)
        (else (in-path? x (cdr path)))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))