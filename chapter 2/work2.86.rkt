#lang sicp

;;following added to Rational package
(put 'sine '(rational)
     (lambda (x)
       (tag (sin (/ (* 1.0 (numer x)) (denom x))))))

(put 'cosine '(rational)
     (lambda (x)
       (tag (cos (/ (* 1.0 (numer x)) (denom x))))))

(put 'arctan '(rational)
     (lambda (x)
       (tag (atan (/ (* 1.0 (numer x)) (denom x))))))

;;following added to Scheme-number package
(put 'sine '(scheme-number)
     (lambda (x) (tag (sin x))))

(put 'cosine '(scheme-number)
     (lambda (x) (tag (cos x))))

(put 'arctan '(scheme-number)
     (lambda (x) (tag (atan x))))