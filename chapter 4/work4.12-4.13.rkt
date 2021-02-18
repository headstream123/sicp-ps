#lang sicp

;; work4.13
;  以下版本unbound删除了所有frame中的variable及其value,原因如下
;  set及lookup都是遍历所有框架进行操作，若只删除first-frame中的内容，相当于
;  没有删除。其他人的理由是unbound是define的逆操作，define只在第一个框架add
;  unbound也应如此,nonsense,define如此做是因为set及lookup只返回第一个值，
;  相当于后面(或者说前面)的frame覆盖了之前的，如此机械的对应太教条了
;  另一个反对意见是出于封装的考虑,这个就不清楚了，有待继续学习，现在还是坚持自己

(define (unbound? exp)
  (tagged-list? exp 'make-unbound!))

(define (make-unbound variable)
  (list 'make-unbound! variable))

(define (unbound-variable exp) (cadr exp))

(define (eval-unbound exp env)
  (let ((var (unbound-variables exp)))
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (cadr vars))
               (set-cdr! vars (cddr vars))
               (set-cdr! vals (cddr vals))
               (env-loop (enclosing-environment env)))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          'done
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))))


;; work4.12
;  思路类似 sicp p184 assoc
(define (env-loop var env)
  (define (scan vars vals)
    (cond ((null? vars)
           (env-loop var (enclosing-environment env)))
          ((eq? var (car vars))
           vals)
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      'false
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (lookup-variable-value var env)
  (let ((vals (env-loop var env)))
    (if vals
        (car vals)
        (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((vals (env-loop var env)))
    (if vals
        (set-car! vals val)
        (error "Unbound variable -- SET!" var))))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((vals (env-loop var frame)))
      (if vals
          (set-car! vals val)
          (add-binding-to-frame! var val frame)))))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))