#lang sicp

(define (filtered-accumulate predicate combiner null-value
                             term a next b)
  (cond ((> a b) null-value)
        ((predicate a)
         (combiner
          (term a)
          (filtered-accumulate predicate combiner null-value
                               term (next a) next b)))
        (else
         (filtered-accumulate predicate combiner null-value
                               term (next a) next b))))

(define (sum-primes a b)
  (define (next x)
    (if (odd? x)
        (+ x 2)
        (+ x 1)))
  (define (identity x) x)
  (filtered-accumulate prime? + 0 identity a next b))

(define (product-primes n)
  (define (coprime? i n)
    (and (< i n)
         (= 1 (gcd i n))))
  (filtered-accumulate
   (lambda (x) (coprime? x n))
   *
   1
   (lambda (x) x)
   2
   (lambda (x) (+ x 1))
   n))

(define (filtered-accumulate1 predicate combiner null-value
                             term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate a)
           (iter (next a) (combiner result (term a))))
          (else
           (iter (next a) result)))
    (iter a null-value)))