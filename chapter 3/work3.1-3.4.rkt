#lang sicp

;work3.3-3.4
(define (make-account balance password)
  (define count (make-accumulator 0))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (begin (count (- (count 0)))
               (cond ((eq? m 'withdraw) withdraw)
                     ((eq? m 'deposit) deposit)
                     (else
                      (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        (begin (count 1)
               (if (= (count 0) 7) (display "call-the-cops"))
               (error "Incorrect password"))))
  dispatch)

;work3.2
(define (make-monitored function)
  (define count (make-accumulator 0))
  (define (dispatch m)
    (cond ((eq? m 'how-many-calls?) (count 0))
          ((eq? m 'reset-count) (count (-(count 0))))
          (else
           (begin (count 1) (function m)))))
  dispatch)

;work3.1
(define (make-accumulator start)
  (lambda (x)
    (begin (set! start (+ start x))
           start)))