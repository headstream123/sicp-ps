#lang sicp

(define (cont-frac n d k)
  (define (rec n d i k)
     (if (> i k)
      0
      (/ (n i) (+ (d i) (rec n d (+ i 1) k)))))
  (rec n d 1 k))

(define (d i)
  (define (sum? i)
    (and (> i 4)
         (= 0 (remainder (- i 2) 3))))
  (cond ((< i 3) i)
        ((sum? i) (+ (d (- i 1))
                     (d (- i 2))
                     (d (- i 3))))
        (else 1.0)))

(define (e k)
  (+ 2 (cont-frac (lambda (x) 1.0) d k)))

(define (tan-cf x k)
  (define (n i)
    (if (= i 1)
        x
        (* x (- x))))
  (cont-frac n
             (lambda (i) (- (* i 2) 1))
             k))