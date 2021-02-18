#lang sicp

;; work4.44
;  答案是row的list,是按照col的逆序排列的
(define (queens board-size)
  (define (queen-cols k result)
    (let ((row (an-integer-between 1 board-size)))
      (if (> k board-size)
          result
          (begin
            (require (safe? row k result))
            (queen-cols (+ k 1) (cons row result))))))
  (queen-cols 1 '()))

(define (safe? row col s)
  (cond ((null? s) #t)
        ((= (car s) row) #f)
        ((= (abs (- (car s) row))
            (abs (- (length s) col))) #f)
        (else (safe? row col (cdr s)))))

;; Auxiliary
(define (require p)
  (if (not p) (amb)))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))