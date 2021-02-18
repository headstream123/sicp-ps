#lang sicp

;; work4.41
;  这题主要是复习，里面用到了书中p84的程序
;  这题还有一个思路很有意思，在这记录一下，就是把这5个数看成5进制的5位数
;  若是考虑一般性(解不唯一)，可以(cons tenant filter...)
(define (multiple-dwelling-usual)
  (let ((floors (permutations (list 1 2 3 4 5)))
        (tenant (list 'baker 'cooper 'fletcher 'miller 'smith)))
    (map
     (lambda (t f) (list t f))
     tenant
     (car
      (filter
       (lambda (s)
         (let ((baker (car s)) (cooper (cadr s))
               (fletcher (caddr s)) (miller (cadddr s))
               (smith (cadddr (cdr s))))
           (and (not (= baker 5))
                (not (= cooper 1))
                (not (= fletcher 1))
                (not (= fletcher 5))
                (not (= (abs (- fletcher cooper)) 1))
                (not (= (abs (- smith fletcher)) 1))
                (> miller cooper))))
      floors)))))

(define (permutations s)
  (if (null? s)                        
      (list nil)                        
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; work4.40
;  果然用嵌套let排除一些可能性后，效率更高
(define (multiple-dwelling-new)
  (let ((cooper (amb 1 2 3 4 5)))
    (require (not (= cooper 1)))
    (let ((fletcher (amb 1 2 3 4 5)))
      (require (not (= fletcher 1)))
      (require (not (= fletcher 5)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (let ((baker (amb 1 2 3 4 5)))
            (require (not (= baker 5)))
            (require
              (distinct?
               (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))

;; work4.39
;  考虑到深度优先搜索，baker != 5其实没甚意义，调到最后，其他的按照顺序排列
;  由于其他要求较多，所以distinct?移到最后，也许可以减少时间
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 1)))
    (require (not (= fletcher 5)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= baker 5)))
    (require
      (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (require p)
  (if (not p) (amb)))

;; Auxiliary
(define (filter predicate sequence) 
   (cond ((null? sequence) nil) 
         ((predicate (car sequence)) 
          (cons (car sequence)  
                (filter predicate (cdr sequence)))) 
         (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high)))) 

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
