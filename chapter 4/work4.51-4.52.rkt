#lang sicp

;; work4.52
(define (analyze exp)
  (cond ((if-fail? exp) (analyze-if-fail exp))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-sequence exp) (cadr exp))
(define (if-fail-alter exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((first (analyze (if-fail-sequence exp)))
        (second (analyze (if-fail-alter exp))))
    (lambda (env succeed fail)
      (first env
             succeed
             (lambda () (second env succeed fail))))))

;; work4.51
;  不需撤销之后,set就和define没有区别了
(define (analyze exp)
  (cond ((permanent-assignment? exp)   ; add
         (analyze-permanent-assignment exp))))

(define (permanent-assignment? exp)
  (tagged-list? exp 'permanent-set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))