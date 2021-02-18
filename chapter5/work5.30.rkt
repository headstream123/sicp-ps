#lang sicp

;; work5.30
;; a)
;; 仅修改了例子中提到的错误, 其余浩大的工程就留待以后吧

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        'Unbound-variable  ; changed
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (test (op equal?) (reg val) (const Unbound-variable)) ; add
  (branch (label signal-error)) ; add
  (goto (reg continue))