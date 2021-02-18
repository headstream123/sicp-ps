#lang sicp

;; work3.72
(define (triple-sums s)
  (let ((s1 (stream-car s))
        (s2 (stream-car (stream-cdr s)))
        (s3 (stream-car (stream-cdr (stream-cdr s))))
        (rest-s (stream-cdr (stream-cdr (stream-cdr s)))))
    (if (= (weight-a s1) (weight-a s2) (weight-a s3))
        (cons-stream
         (list (weight-a s1) s1 s2 s3)
         (triple-sums rest-s))
        (triple-sums (stream-cdr s)))))

(define (weight-a p)
  (+ (square (car p)) (square (cadr p))))

;; work3.71
(define (ramanujan-number s)
  (let ((scar (stream-car s))
        (scadr (stream-car (stream-cdr s))))
    (if (= (weight scar) (weight scadr))
        (cons-stream
         (list (weight scar) scar scadr)
         (ramanujan-number (stream-cdr s)))
        (ramanujan-number (stream-cdr s)))))

(define (weight p)
  (+ (expt (car p) 3) (expt (cadr p) 3)))

;; auxiliary
(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((> (weight s1car) (weight s2car))
                  (cons-stream
                   s2car
                   (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream
                   s1car (merge-weighted (stream-cdr s1)
                                         s2
                                         weight))))))))

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

;; definition
(define r
  (ramanujan-number
   (weighted-pairs integers integers weight)))

(define a
  (triple-sums
   (weighted-pairs integers integers weight-a)))