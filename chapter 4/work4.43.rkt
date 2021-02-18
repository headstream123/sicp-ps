#lang sicp

;; work4.43
;  very ugly,主要是写的不习惯,一眼就看出来的答案,结果写了一大段
;  也许用father search daughter会好一点?
;  要是没有Moore,会有3个解,moore downing parker
(define (lorna-father)
  (let ((mary 'moore)
        (melissa 'hood)
        (owner-yacht
         (list (list 'hood 'gabrielle) (list 'moore 'lorna)
               (list 'hall 'rosalind) (list 'parker 'mary)
               (list 'downing 'melissa))))
    (let ((rosalind (amb 'downing 'hall 'parker)))
      (require (not (equal? rosalind 'hall)))
      (let ((gabrielle (amb 'downing 'hall 'parker)))
        (require (not (equal? gabrielle 'parker)))
        (let ((lorna (amb 'downing 'hall 'parker)))
          (require
            (distinct?
             (list rosalind gabrielle lorna)))
          (require
            (equal?
             (cadar (filter
                     (lambda (p) (equal? gabrielle (car p)))
                     owner-yacht))
             (if (equal? 'parker rosalind)
                 'rosalind
                 'lorna)))
          lorna)))))

;; Auxiliary
(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (require p)
  (if (not p) (amb)))

(define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence)))))