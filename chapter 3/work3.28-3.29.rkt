#lang sicp

;; work3.29
;; time: and-gate-delay + 2 * inverter-delay 
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((ai1 (make-wire))
          (ai2 (make-wire)) (oa (make-wire)))
      (inverter a1 ai1)
      (inverter a2 ai2)
      (and-gate ai1 ai2 oa)
      (inverter oa output)))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and x y)
  (if (and (= x 1) (= y 1))
      1
      0))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

;; work3.28
(define (or-gate1 a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
        ((or (= a 1) (= b 1)) 1)
        (else (error "Invalid signal" a b))))