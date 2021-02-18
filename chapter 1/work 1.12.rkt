#lang sicp

(define (pascal-rec row col)
  (cond ((> col row) (error"error"))
        ((or (= col 0) (= col row)) 1)
        (else (+ (pascal-rec (- row 1) col)
                 (pascal-rec (- row 1) (- col 1))))))