#lang sicp

(define (sum-distinct-ints n s)
  (define (equal-sum? pair)
    (= (+ (car pair) (cadr pair) (caddr pair)) s))
  (map make-pair-sum
       (filter equal-sum?
               (different-pairs n))))

(define (different-pairs n)
  (let ((list-n (enumerate-interval 1 n)))
    (flatmap
     (lambda (i)
       (flatmap
        (lambda (j)
          (map (lambda (k) (list i j k))
               (remove j (remove i list-n))))
        (remove i list-n)))
     list-n)))

(define (different-pairs1 n)
  (flatmap
   (lambda (i)
     (map
      (lambda (j) (list i j))
      (unique-pairs (- i 1)))
     (enumerate-interval 1 n))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (caddr pair)
        (+ (car pair) (cadr pair) (caddr pair))))

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