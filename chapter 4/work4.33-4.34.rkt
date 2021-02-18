#lang sicp

;; work4.34
;  非常偷懒，就在user-print里加了个if，只能判定输入的最外层的cons
;  要实现题中要求，要修改求值器，将cons实现为非严格的，有待以后完成吧
(define (driver-loop) 
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print input output)))  ; changed
  (driver-loop)) 

(define (user-print input object)
  (if (compound-procedure? object)
      (if (and (pair? input) (eq? (car input) 'cons))
          (display (cons (cadr input) (caddr input))) ; add
          (display (list 'compound-procedure
                         (procedure-parameters object)
                         (procedure-body object)
                         '<procedure-env>)))
      (display object)))

;; work4.33
;  使用时需要将eval中quote相关求值语句改为(text-of-quotation exp env)
(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
        (eval (lazy-text text) env)
        text)))

(define (lazy-text text)
  (if (pair? text)
      (list 'cons
            (list 'quote (lazy-text (car text)))
            (lazy-text (cdr text)))
      text))