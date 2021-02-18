#lang sicp

;work3.19 可惜不是自己想出来的，很精巧的构思，“套圈思想”。
(define (loop? x)
  (define (safe-cdr x)
    (if (pair? x) (cdr x) '()))
  (define (iter s d)
    (cond((not (and (pair? s) (pair? d))) #f)
         ((eq? s d) #t)
         (else (iter (safe-cdr s) (safe-cdr (safe-cdr d))))))
  (iter x (safe-cdr x)))

;work3.18
(define cycle?
  (let ((path '()))
    (lambda (x)
      (if (not (pair? x))
          #f
          (if (memq x path)
              #t
              (begin (set! path (cons x path))
                     (cycle? (cdr x))))))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle (list 'a 'b 'c)))