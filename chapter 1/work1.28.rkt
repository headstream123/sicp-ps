#lang sicp

(define (prime? n)
  (fast-prime? n 10))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (miller-rabin-test n)
  (define (try-it n a)
    (if (= (expmod a (- n 1) n) 0)
        #f
        (= (remainder 1 n) (expmod a (- n 1) n))))
  (try-it n (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (and (= 1 (square (expmod base (/ exp 2) m)))
                  (not (= (expmod base (/ exp 2) m) 1))
                  (not (= (expmod base (/ exp 2) m) (- m 1))))
             0
             (remainder (square (expmod base (/ exp 2) m)) m)))
        ( else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (square n) (* n n))