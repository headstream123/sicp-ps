#lang sicp

;; work4.31
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (apply-meta (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply-meta procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) 
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (real-procedure-parameters procedure) ; changed
           (list-of-delayed-args
            (procedure-parameters procedure)
            arguments
            env) ;changed list-of-delayed-args 
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args paras args env)
  (define (dispatch para arg)
    (cond ((not (pair? para)) (actual-value arg))
          ((eq? (cadr para) 'lazy) (delay-it arg))
          ((eq? (cadr para) 'lazy-memo) (delay-memo arg))
          (else 'false)))
  (let ((first (car paras))
        (rest (cdr paras)))
    (cond ((no-operands? args) '())
          ((dispatch first (first-operand args))
           (cons (dispatch first (first-operand args))
                 (list-of-delayed-args
                  rest (rest-operands args) env)))
          (else (error "Parameter error" first)))))

;; thunks
(define (delay-it exp env)
  (list 'thunk exp env))

(define (delay-memo exp env)
  (list 'thunk-memo exp env))  ; addition

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-memo? obj)
  (tagged-list? obj 'thunk-memo)) ; addition

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj))) ; add
        ((thunk-memo? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())     
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;; evaluator data structures
(define (real-procedure-parameters p)
  (map (lambda (x) (if (pair? x) (car x) x))(cadr p))) ; add