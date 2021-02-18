#lang sicp

;; work3.76
(define (make-zero-crossings input-stream smooth)
  (let ((s1 (smooth input-stream)))
    (stream-map sign-change-detector
                s1
                (cons-stream (stream-car s1) s1))))

(define (smooth s)
  (stream-map (lambda (s1 s2) (/ (+ s1 s2) 2))
              s (stream-cdr s)))

;; work3.75
(define (make-zero-crossings1 input-stream last-avpt prev)
  (let ((avpt (/ (+ (stream-car input-stream) prev) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossings1
                  (stream-cdr input-stream)
                  avpt
                  (stream-car input-stream)))))

;; work3.73
(define (RC R C dt)
  (lambda (stream-i v0)
    (add-streams
     (scale-stream stream-i R)
     (integral (scale-stream stream-i (/ 1.0 C))
               v0 dt))))

;; auxiliary
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

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

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc (map stream-cdr argstreams))))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (square x) (* x x))