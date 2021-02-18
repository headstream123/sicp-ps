#lang sicp

;; work4.1
(define (list-of-values-l2r exps env)
  (if (no-operands? exps)
      '()
      (let ((first-value (eval (first-operand exps) env)))
      (cons first-value
            (list-of-values-l2r (rest-operands exps) env)))))

(define (list-of-values-r2l exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-r2l (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            right))))

;(define (list-of-values-r2l exps env)
  ;(list-of-values-l2r (reverse exps) env))
