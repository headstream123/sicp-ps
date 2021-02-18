#lang sicp

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

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))