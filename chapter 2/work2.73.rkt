#lang sicp

(define (install-exp-package)
  ;;internal procedures
  (define (base e) (car e))
  (define (exponent e) (cadr e))
  (define (make-exp b e)
    (cond ((=number? e 0) 1)
          ((and (=number? b 0) (number? e) (< e 0))
           (error "div zero error"))
          ((=number? b 0) 0)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))
  (define (deriv-exp operands var)
    ((get 'make-product '*)
     (exponent operands)
     ((get 'make-product '*)
      (make-exp (base operands)
                (- (exponent operands) 1))
      (deriv (base operands) var))))
  ;;interface to the rest of the system
  (put 'base '** base)
  (put 'exponent '** exponent)
  (put 'make-exp '** make-exp)
  (put 'deriv-exp '** deriv-exp)
  'done)

(define (install-mul-package)
  ;;internal procedures
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (deriv-mul operands var)
    ((get 'make-sum '+)
     (make-product (multiplier operands)
                   (deriv (multiplicand operands) var))
     (make-product (deriv (multiplier operands) var)
                   (multiplicand operands))))
  ;;interface to the rest of the system
  (put 'multiplier '* multiplier)
  (put 'multiplicand '* multiplicand)
  (put 'make-product '* make-product)
  (put 'deriv-mul '* deriv-mul)
  'done)

(define (install-sum-package)
  ;;internal procedures
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  ;;interface to the rest of the system
  (put 'addend '+ addend)
  (put 'augend '+ augend)
  (put 'make-sum '+ make-sum)
  (put 'deriv-sum '+ deriv-sum)
  'done)

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))



