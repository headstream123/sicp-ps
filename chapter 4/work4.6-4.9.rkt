#lang sicp

;; work4.9
;  while (int i < n){<body>}
;  (while predicate body)
(define (while->combination exp env)
  (let ((predicate (car exp))
        (body (cadr exp)))
    (if (null? exp)
        'false
        (sequence->exp
         (list
          (list 'define
                (list 'while-iter)
                (make-if
                 predicate
                 (sequence-exp (list body (list 'while-iter)))
                 'true))
          (list 'while-iter))))))

;  for (int i = 0; i < n; i++){<body>}
;  (for (assignment predicate control) body)
;  for未完成，下面引用了自身，仅是思路
(define (for-eval exp env)
  (let ((assignment (caar exp))
        (predicate (cadar exp))
        (control (caddar exp))
        (body (cadr exp)))
    ((eval assignment env)
     (make-if predicate
              (list
               (sequence->exp body)
               (for-eval
                (list (cons control predicate control) body)
                env))
             'true))))

;; work4.7
(define (let*->nested-lets exp)
  (define (let* seqs)
    (if (null? seqs)
        (sequence->exp (cdr exp))
        (let ((first (car seqs))
              (rest (cdr seqs)))
          (if (null? rest)
              (make-let first (cdr exp))
              (make-let first (let* rest))))))
  (if (null? exp) 'false (let* (car exp))))

;; work4.6 4.8
;  刻意没有使用map和let，故程序较繁琐，其实let应该可以用吧，否则何谈元循环
;  4.8有的答案加上了sequence->exp，但我认为这已经是一个exp了
(define (install-let-package)
  ;;internal procedures
  (define (let-body clauses) (cdr clauses))
  (define (first-clause clauses) (caar clauses))
  (define (rest-clauses clauses) (cdar clauses))
  
  (define (list-of-let clauses)
    (if (null? clauses)
      (list '() '())
      (list (cons (car (first-clause clauses))
                  (list-of-let (rest-clauses clauses)))
            (cons (cadr (first-clause clauses))
                  (list-of-let (rest-clauses clauses))))))
  
  (define (let->combination clauses)
    (if (= (length clauses) 3)
        (list
         (list 'define
               (cons (car clauses) (car (list-of-let clauses)))
               (let-body clauses))
         (cons (car clauses) (cadr (list-of-let clauses))))
        (list (make-lambda (car (list-of-let clauses))
                           (let-body clauses))
          (cadr (list-of-let clauses)))))
  
  (define (eval-let exp env)
    (if (null? exp)
        'false
        (eval (let-combination exp) env)))

  (define (make-let sequence body)
    (cons 'let (cons sequence body)))
  ;;interface to the rest of the system
  (put 'eval 'let eval-let)
  (put 'make 'let make-let)
  'done)
(define (make-let sequence body)
  ((get 'make 'let) sequence body))