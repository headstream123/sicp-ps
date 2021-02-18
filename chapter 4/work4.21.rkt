#lang sicp

;; work4.21
;; b)
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

;; a)
(define (fib-rec n)
 ((lambda (fib) (fib fib n))
  (lambda (fb k)
    (if (or (= k 0) (= k 1))
        k
        (+ (fb fb (- k 1)) (fb fb (- k 2)))))))

(define (fib-iter n)
  ((lambda (fib) (fib fib 1 0 n))
   (lambda (fb a b counter)
     (if (= counter 0)
         b
         (fb fb (+ a b) a (- counter 1))))))