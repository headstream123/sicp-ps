#lang sicp

(define (make-joint account acc-key new-key)
  (lambda (p m)
    (if (eq? p new-key)
        (account acc-key m)
        (error "Wrong password"))))

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

(define (make-accumulator start)
  (lambda (x)
    (begin (set! start (+ start x))
           start)))