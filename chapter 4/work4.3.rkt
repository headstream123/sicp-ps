#lang sicp

(define (install-cond-package)
  ;;internal procedures
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

  (define (cond-predicate clause) (car clause))
  (define (cond-actions clause) (cdr clause))  

  (define (cond->if exp) (expand-clauses exp))

  (define (expand-clauses clauses)
    (if (null? clauses)
        'false                          ; no else clause
        (let ((first (car clauses))
              (rest (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last -- COND->IF"
                         clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))

  (define (eval-cond exp env)
    (eval (cond->if exp) env))
  
  ;;interface to the rest of the system
  (put 'eval 'cond eval-cond)
  'done)


(define (install-begin-package)
  ;;internal procedures
  (define (last-exp? seq) (null? (cdr seq)))
  (define (first-exp seq) (car seq))
  (define (rest-exps seq) (cdr seq))
  
  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))

  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq) (first-exp seq))
          (else (make-begin seq))))

  (define (make-begin seq) (cons 'begin seq))
  
  ;;interface to the rest of the system
  (put 'vary 'begin sequence->exp)
  (put 'make 'begin make-begin)
  (put 'eval 'begin eval-sequence)
  'done)
(define (sequence->exp seq)
  ((get 'vary 'begin) seq))


(define (install-lambda-package)
  ;;internal procedures
  (define (lambda-parameters exp) (car exp))
  
  (define (lambda-body exp) (cdr exp))
  
  (define (eval-lambda exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env))
  
  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body)))
  
  ;;interface to the rest of the system
  (put 'make 'lambda make-lambda)
  (put 'eval 'lambda eval-lambda)
  'done)
(define (make-lambda parameters body)
  ((get 'make 'lambda) parameters body))


(define (install-if-package)
  ;;internal procedures
  (define (if-predicate exp) (car exp))
  
  (define (if-consequent exp) (cadr exp))
  
  (define (if-alternative exp)
    (if (not (null? (cddr exp)))
        (caddr exp)
        'false))

  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

  (define (make-if predicate consequent alternative)
    (list 'if predicate consequent alternative))
  
  ;;interface to the rest of the system
  (put 'make 'if make-if)
  (put 'eval 'if eval-if)
  'done)
(define (make-if predicate consequent alternative)
  ((get 'make 'if) predicate consequent alternative))


(define (install-definition-package)
  ;;internal procedures
  (define (definition-variable exp)
    (if (symbol? (car exp))
        (car exp)
        (caar exp)))
  
  (define (definition-value exp)
    (if (symbol? (car exp))
        (cadr exp)
        (make-lambda (cdar exp) (cdr exp))))
  
  (define (eval-definition exp env)
    (define-variable!
      (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)

  ;;interface to the rest of the system
  (put 'eval 'define eval-definition)
 'done)


(define (install-assignment-package)
  ;;internal procedures
  (define (assignment-variable exp) (car exp))

  (define (assignment-value exp) (cadr exp)) 
  
  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)
  
  ;;interface to the rest of the system
  (put 'eval 'set! eval-assignment)
  'done)


(define (install-quote-package)
  ;;internal procedures
  (define (text-of-quotation exp) (car exp))

  (define (eval-quote exp env)
    (text-of-quotation exp))
  
  ;;interface to the rest of the system
  (put 'eval 'quote eval-quote)
  'done)


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (car exp))  ;将函数当谓词用
         ((get 'eval (tag exp)) (content exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (tag exp) (car exp))
(define (content exp) (cdr exp))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
