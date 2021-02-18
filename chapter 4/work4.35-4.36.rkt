#lang sicp

;; work4.36
;; indirect
(define (a-pythagorean-triple)
  (define (rec low high)
    (amb (a-pythagorean-triple-between low high)
         (rec low (+ high 1))))
  (rec 1 1))

;; direct
(define (a-pythagorean-triple-than low)
  (let ((k (an-integer-starting-from low)))
    (let ((i (an-integer-between low k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

;; work4.35
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (require p)
  (if (not p) (amb)))