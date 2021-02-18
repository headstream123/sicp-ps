#lang sicp

;; work5.13
;  根据题意如此改动是最简便的,但是个人认为不如未改动前版本严谨
(define (lookup-register name)
  (let ((val (assoc name register-table)))
    (if val
        (cadr val)
        (begin (allocate-register name)
               (lookup-register name)))))