#lang sicp

;work2.84-2.85
(define (drop x)
  (let ((tx (type-tag x)))
    (let ((px ((get 'project tx) x)))
      (cond ((equal? tx 'scheme-number) x)
            ((equ? ((get 'raise (type-tag px)) px) px)
             (drop px))
            (else x)))))

;;following added to Complex package
(put 'project 'complex
     (lambda (x)
       (make-real
        ((get 'real-part '(complex)) x))))

;;following added to Real package
(define (project x)
  (if (= (round (* x 10)) (* x 10))
      ((get 'make 'rational) (* x 10) 10)
      (project (* x 10))))

(put 'project 'real project)

;;following added to Rational package
(put 'project 'rational
     (lambda (x)
       (make-scheme-number
        (round (/ (* 1.0 (numer x)) (denom x))))))

(define (less-than? a b)
  (let ((ta (type-tag a))
        (tb (type-tag b)))
    (cond ((equal? ta 'complex) #f)
          ((equal? ((type-tag (get 'raise ta) a) tb)) #t)
          (else (less-than? ((get 'raise ta) a) b)))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types"
                                (list op type-tags))
                    (let ((t1->t2 (get 'raise type1))
                          (t2->t1 (get 'raise type2)))
                      (if (less-than? a1 a2)
                          (apply-generic op (t1->t2 a1) a2)
                          (apply-generic op a1 (t2->t1 a2))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;work2.83
(put 'raise 'real
     (lambda (x) (make-complex-from-real-imag x 0)))

(put 'raise 'rational
     (lambda (x)
       ((get 'make 'real) (/ (numer x) (denom x) 1.0))))

(put 'raise 'scheme-number
     (lambda (x) (get 'make 'rational) x 1))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))