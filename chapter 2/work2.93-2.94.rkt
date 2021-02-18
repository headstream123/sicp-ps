#lang sicp

(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1) (term-list p2)))
       (error "Polys not in same var -- GCD-POLY"
             (list p1 p2))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

(define (remainder-terms t1 t2)
    (cadr (div-terms t1 t2)))

(put 'gcd '(polynomial polynomial)
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))
(put 'gcd '(scheme-number scheme-number)
     (lambda (x y) (tag (gcd x y))))

(define (greatest-common-divisor x y)
  (apply-generic 'gcd x y))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (make-rat n d)
    (let ((g (gcd-poly n d)))
      (cons (div n g) (div d g))))
  
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= (numer x) 0)))
  (put 'negative 'rationnal
       (lambda (x)
         (make-rational
          (negative (numer x)) (denom x))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))