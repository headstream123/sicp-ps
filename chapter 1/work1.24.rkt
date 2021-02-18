#lang sicp

(define (prime? n)
  (fast-prime n 10))

(define (fast-prime n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it n a)
    (= (expmod a n n) a))
  (try-it n (+ 1 (random (- n 1)))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        ( else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (square n) (* n n))

(define (search-for-primes n)
  (start-prime-test n 3 (runtime)))

(define (start-prime-test n count start-time)
  (continue-primes n count)
  (report-prime (- (runtime) start-time)))

(define (report-prime elapsed-time)
  (newline)
  (display " *** ")
  (display elapsed-time))

(define (continue-primes n count)
  (cond ((= count 0) (display "are primes."))
        ((prime? (next-odd n))
         (display (next-odd n))
         (newline)
         (continue-primes (next-odd n) (- count 1)))
        (else
         (continue-primes (next-odd n) count))))

(define (next-odd n)
  (if (= (remainder n 2) 0)
      (+ n 1)
      (+ n 2)))