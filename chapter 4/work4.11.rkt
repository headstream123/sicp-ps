#lang sicp

;; work4.11
;  由于没有表头，只能将add-binding-to-frame!中的新binding插入到第二位
;  若是加入'*frame*这种表头，操作会更合理，但为了简洁(tou lan)未加上
(define (make-frame variables values)
  (map cons variables values))

(define (first-binding frame) (car frame))
(define (rest-bindings frame) (cdr frame))

(define (make-binding variable value)
  (cons variable value))

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame
            (cons (make-binding var val)
                  (rest-bindings frame))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (first-binding frame)))
             (cdr (first-binding frame)))
            (else (scan (rest-bindings frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-environment env)))
            ((eq? var (car (first-binding frame)))
             (set-cdr! (car (first-binding frame)) val))
            (else (scan (rest-bindings frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (define (scan frame)
    (cond ((null? frame)
           (add-binding-to-frame! var val frame))
          ((eq? var (car (first-binding frame)))
           (set-cdr! (car (first-binding frame)) val))
          (else (scan (rest-bindings frame)))))
    (scan (first-frame env)))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))