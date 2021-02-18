#lang sicp

;; work4.20
;; a)
(define (letrec? exp)
  (tagged-list? (car exp) 'letrec))

(define (letrec-clauses exp) (cadr exp))
(define (letrec-body exp) (caddr exp))

(define (clause-variable clause) (car clause))
(define (clause-value clause) (cadr clause))

(define (letrec->let exp)
  (let ((clauses (letrec-clauses exp))
        (body (letrec-body exp)))
    (let ((vars (map clause-variable clauses))
          (vals (map clause-value clauses)))
      (make-let
       (map (lambda (x) (list x '*unassigned*)) vars)
       (append
        (map (lambda (x y) (list 'set! x y)) vars vals)
        body)))))

(define (eval-letrec exp env)
  (eval (letrec->let exp) env))