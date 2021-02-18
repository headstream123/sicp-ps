#lang sicp

;; work3.62
(define (div-series s1 s2)
  (let ((c (stream-car s2)))
    (if (= c 0)
        (error "DIV ZERO ERROR" s2)
      (mul-series (scale-stream s1 c)
                  (reciprocal-series
                   (scale-stream s2 (/ 1 c)))))))

(define tangent-series
  (div-series sine-series cosine-series))

;; work3.61
(define (reciprocal-series s)
  (cons-stream 1 (mul-series (scale-stream (stream-cdr s) -1)
                             (reciprocal-series s))))

;; work3.60 下面的注释版本是我的思路，个人感觉书上版本可读性更强
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))
  ;(add-streams (scale-stream s2 (stream-car s1))
               ;(cons-stream 0 (mul-series (stream-cdr s1) s2))
               ;))

;; work3.59
; b)
(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series
  (cons-stream
   1 (integrate-series (scale-stream sine-series -1))))

;; 回代版本，完全没必要，一开始的版本就可以运行
(define cosine-series1
  (cons-stream
   1 (scale-stream
      (integrate-series
       (cons-stream 0 (integrate-series cosine-series1)))
      -1)))

(define sine-series1
  (cons-stream
   0 (integrate-series
      (cons-stream
       1 (scale-stream (integrate-series sine-series1) -1)))))

; a)
(define (integrate-series s)
  (div-streams s integers))

;; auxiliary

(define ones (cons-stream 1 ones))

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define x (integers-starting-from 5))
(define y (integers-starting-from 2))

(define a (mul-series x y))
(define b (add-streams
           (mul-series sine-series sine-series)
           (mul-series cosine-series cosine-series)))
(define c (reciprocal-series cosine-series))