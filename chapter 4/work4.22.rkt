#lang sicp

;; work4.22
;  还可以直接使用4.6中的定义，只需在analyze里加上
;  ((let? exp) (analyze (let->combination exp))) 
(define (let? exp)
  (tagged-list? exp 'let))

(define (let-clauses exp) (cadr exp))
(define (let-body exp) (caddr exp))
(define (clause-variables clauses) (map car clauses))
(define (clauses-values clauses) (map cadr clauses))

(define (analyze-let exp)
  (let ((body (let-body exp))
        (vars (clause-variables (let-clauses exp)))
        (vprocs (map analyze (clause-values (let-clauses exp)))))
    (lambda (env)
      (list (make-lambda vars body)
            (map (lambda (vproc) (vproc env)) vprocs)))))

