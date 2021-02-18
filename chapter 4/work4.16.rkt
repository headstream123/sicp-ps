#lang sicp

;; work4.16
;; c)
;  scan-out-defines应安装在make-procedure中，因为若安装在procedure-body
;  中，每次调用procedure-body，scan-out-defines都会运行。
(define (make-procedure parameters body env)
  (list 'procedure
        parameters (scan-out-defines body) env))

;; b)
(define (scan-out-defines procedure-body)
  (define (out-defines defines body)
    (let ((vars (map definition-variable defines))
          (vals (map definition-value defines)))
      (make-let
       (map (lambda (x) (list x '*unassigned*)) vars)
       (append
        (map (lambda (x y) (list 'set! x y)) vars vals)
        body))))
  (define (define? proc-body)
    (let ((symbol (caar proc-body))
          (rest (cdr proc-body)))
      (cond ((null? rest) (eq? symbol 'define))
            ((eq? symbol 'define) 'true)
            (else (scan rest)))))
  (cond ((null? procedure-body)
         (error ("Empty procedure body")))
        ((define? procedure-body)
         (out-defines
          (filter
           (lambda (x) (eq? (car x) 'define)) procedure-body)
          (filter
           (lambda (x) (not (eq? (car x) 'define)))
           procedure-body)))
        (else procedure-body)))

;; a)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; auxiliary
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))