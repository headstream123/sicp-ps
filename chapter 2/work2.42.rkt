#lang sicp

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list nil)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (safe? col s)
  (define (unequal? r c s)
    (cond ((null? s) #t)
          ((= (caar s) r) #f)
          ((= (abs (- (caar s) r))
              (abs (- (cadar s) c))) #f)
          (else (unequal? r c (cdr s)))))
  (unequal? (row-value s col)
            col
            (remove-pair (row-value s col) col s)))

(define (remove-pair row col s)
  (filter (lambda (x)
            (not (and (= (car x) row)
                      (= (cadr x) col))))
          s))

(define (row-value s col)
  (cond ((null? s) 0)
        ((= (cadar s) col) (caar s))
        (else (row-value (cdr s) col))))

(define (adjoin-position row col s)
  (append (list (list row col)) s))

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
