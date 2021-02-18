#lang sicp

;; work4.4
(define (install-or-package)
  ;;internal procedures
  (define (or->if exp)
    (if (null? exp)
        'false
        (let ((first (car exp))
              (rest (cdr exp)))
          (make-if first 'true (or->if rest)))))

  (define (eval-or exp env)
    (eval (or->if exp) env))
  
  (define (eval-or-direct exp env)
     (if (null? exp)
        'false
        (let ((first (car exp))
              (rest (cdr exp)))
          (if (true? (eval first env))
              'true
              (eval-or-direct rest env)))))
  
  ;;interface to the rest of the system
  (put 'eval 'or eval-or)
  'done)


(define (install-and-package)
  ;;internal procedures
  (define (and->if exp)
    (if (null? exp)
        'true
        (let ((first (car exp))
              (rest (cdr exp)))
          (if (null? rest)
              (sequence->exp first)
              (make-if first (and->if rest) 'false)))))

  (define (eval-and exp env)
    (eval (and->if exp) env))
  
  (define (eval-and-direct exp env)
     (if (null? exp)
        'true
        (let ((first (car exp))
              (rest (cdr exp)))
          (if (not (eval first env))
              'false
              (if (null? rest)
                  (eval first env)
                  (eval-and-direct rest env))))))
  
  ;;interface to the rest of the system
  (put 'eval 'and eval-and)
  'done)