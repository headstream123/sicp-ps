#lang sicp

;; work4.26
;; a)
(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-condition exp) (cadr exp))
(define (unless-exceptional-value exp) (caddr exp))
(define (unless-usual-value exp) (cadddr exp))

(define (unless->if exp)
  (let ((condition (unless-condition exp))
        (exceptional-value (unless-exceptional-value exp))
        (usual-value (unless-usual-value exp)))
    (make-if condition exceptional-value usual-value)))

(define (eval-unless exp env)
  (eval (unless->if exp) env))

;; b)
;  如上所示，unless被定义为特殊形式后，就不能使用map/apply等高阶过程了
;  因为它变成了语法形式，不是过程了，譬如要对两个list选择执行，就可以使用unless

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
