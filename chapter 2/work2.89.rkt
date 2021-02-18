#lang sicp

;;(1 2 0 3 -2 -5)
(define (adjoin-term term tl)
  (if (=zero? (coeff term))
      tl
      (let ((tf (first-term tl)) (tr (rest-terms tl)))
        (cond ((or (empty-termlist? tl)
                   (> (order term) (order tf)))
             (adjoin-term term (cons 0 tl)))
            ((= (order term) (order tf))
             (cons (+ (coeff term) (coeff tf)) tr))
            ((< (order term) (order tf))
             (cons (coeff tf)
                   (adjoin-term term (cons 0 tr))))))))

(define (the-empty-termlist) '())
(define (first-term term-list)
  (make-term (- (length (term-list)) 1) (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))