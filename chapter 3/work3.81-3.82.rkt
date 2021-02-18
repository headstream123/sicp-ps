#lang sicp

;; work3.82
(define (estimate-integral p x y)
  (let ((prop (monte-carlo (p x y) 0 0)))
    (stream-map (lambda (x) (* 4 x)) prop)))

(define (p-stream x y)
  (stream-map
   (lambda (a b)
     (<= (+ (square (- a x)) (square (- b y))) 1))
   (random-numbers-in-range (- x 1) (+ x 1))
   (random-numbers-in-range (- y 1) (+ y 1))))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1.0) failed)
      (next passed (+ failed 1.0))))

(define (random-numbers-in-range low high)
  (let ((range (- high low)))
    (cons-stream
     (+ low (random (exact->inexact range)))
     (random-numbers-in-range low high))))

;; work3.81
(define (random-numbers-new request-stream)
  (define rand
    (cons-stream
     3
     (stream-map
      (lambda (m s)
        (cond ((eq? 'reset (car m)) (cdr m))
              ((eq? 'generate (car m)) (rand-update s))
              (else (error "Unknown request -- RAND" m))))
      request-stream rand)))
  (stream-cdr rand))

;; auxiliary
(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

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
(define m (cons-stream (cons 'generate '())
                       (cons-stream (cons 'reset 3)
                                    (cons-stream
                                     (cons 'generate '())
                                     m))))

(define pi (estimate-integral p-stream 2 3))