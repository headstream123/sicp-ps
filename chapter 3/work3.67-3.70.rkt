#lang sicp

;; work3.70
(define (weight-b pair)
  (+ (* 2 (car pair)) (* 3 (cadr pair))
     (* 5 (car pair) (cadr pair))))

(define (weight-a pair)
  (+ (car pair) (cadr pair)))

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

;; work3.69
(define (pythagorean-triple)
  (stream-filter
   (lambda (x)
     (= (+ (square (car x)) (square (cadr x)))
        (square (caddr x))))
   (triples integers integers integers)))

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map
     (lambda (x) (cons (stream-car s) x))
     (stream-cdr (pairs t u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))


(define (triples1 s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (interleave
     (stream-map
      (lambda (x) (list (stream-car s) (stream-car t) x))
      (stream-cdr u))
     (stream-map
      (lambda (x)
        (list (stream-car s) (stream-car (stream-cdr t)) x))
      (stream-cdr u)))
    (triples1 (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

;; work3.67
;  pairs-new是按照本人思路简单解法，all-pairs是按照书中提示思路解法
;  经过验证书中解法虽然繁琐，但是正逆序pair分布均匀，实用性更强
(define (pairs-new s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-new (stream-cdr s) t))))

(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t)))
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s)))))

;; work3.66
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

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
(define a (weighted-pairs integers integers weight-a))

(define (fs-235 s)
  (stream-filter
   (lambda (x)
     (not (or (= (remainder x 2) 0)
              (= (remainder x 3) 0)
              (= (remainder x 5) 0))))
   s))

(define b
  (weighted-pairs (fs-235 integers) (fs-235 integers)
                  weight-b))