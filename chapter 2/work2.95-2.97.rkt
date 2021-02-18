#lang sicp

;work2.97
(define (reduce x y)
  (apply-generic 'reduce x y))

(put 'reduce '(scheme-number scheme-number) reduce-integers)
(put 'reduce '(polynomial polynomial) reduce-poly)

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((ans (reduce-terms
                    (term-list p1) (term-list p2))))
          (list
           (make-poly (variable p1) (car ans))
           (make-poly (variable p2) (cadr ans))))
        (error "Polys not in same var -- REDUCE-POLY"
               (list p1 p2))))

(define (reduce-terms n d)
  (let ((gcd (gcd-terms n d))
        (o1 (order (first-term n)))
        (o2 (order (first-term d))))
    (let ((ng (pseudodivision-terms n gcd (max o1 o2)))
          (dg (pseudodivision-terms d gcd (max o1 o2))))
      (let ((g (apply gcd (map coeff (append ng dg)))))
        (list (map (lambda (x) (/ (coeff x) g)) ng)
              (map (lambda (x) (/ (coeff x) g)) dg))))))

(define (pseudodivision-terms t1 t2 o)
  (let ((o2 (order (first-term t2)))
        (c (coeff (first-term t2))))
    (div-terms
     (mul-term-by-all-terms
      (make-term 0 (expt c (+ 1 (- o o2)))) t1)
     t2)))

; work2.96
(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (gcd-terms (term-list p1) (term-list p2)))
       (error "Polys not in same var -- GCD-POLY"
              (list p1 p2))))

;; 考虑到题中涉及的多项式均为整数系数，直接使用了'/。若为稳妥起见，可用
;;(div-terms a (adjoin-term (make-term 0 g) the-empty-term))
(define (gcd-terms a b)
  (if (empty-termlist? b)
      (let ((g (apply gcd (map coeff a))))
        (map (lambda (x) (/ (coeff x) g)) a))
      (gcd-terms b (pseudoremainder-terms a b))))

(define (pseudoremainder-terms t1 t2)
  (let ((o1 (order (first-term t1)))
        (o2 (order (first-term t2)))
        (c (coeff (first-term t2))))
    (cadr (div-terms
           (mul-term-by-all-terms
            (make-term 0 (expt c (+ 1 (- o1 o2)))) t1)
           t2))))

(define (remainder-terms t1 t2)
    (cadr (div-terms t1 t2)))
     
(put 'gcd '(polynomial polynomial)
     (lambda (p1 p2) (tag (gcd-poly p1 p2))))
(put 'gcd '(scheme-number scheme-number)
     (lambda (x y) (tag (gcd x y))))

(define (greatest-common-divisor x y)
  (apply-generic 'gcd x y))

(define (div-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (div-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- DIV-POLY"
             (list p1 p2))))

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     (div-terms
                      (add-terms L1
                                 (nagetive
                                  (mul-terms
                                  (make-term new-o new-c) L2)))
                      L2)))
                (list (adjoin-term (make-term new-o new-c)
                                   (car rest-of-result))
                      (cadr rest-of-result))))))))

(define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  
  (define (make-rat n d)
    (let ((s (reduce n d)))
      (cons (car s) (cadr s))))
  
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