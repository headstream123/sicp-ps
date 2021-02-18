#lang sicp

;; delay-full-adder
;; = 2 * half-adder + or-gate = 2*(or-gate + and-gate)+or-gate
;; = 3 * or-gate + 2 * and-gate|answer = 3n*or-gate+2n*and-gate 
(define (ripple-carry-adder an bn sn c)
  (let ((a (car an)) (b (car bn))
        (s (car sn)) (c-in (make-wire)))
    (if (null? an)
        'ok
        (begin (set-signal! c-in (get-signal c))
               (full-adder a b c-in s c)
               (ripple-carry-adder
                (cdr an) (cdr bn) (cdr sn) c)))))

(define (ripple-carry-adder1 an bn sn c)
  (define (iter an bn sn value-of-c)
    (let ((a (car an)) (b (car bn))
          (s (car sn)) (c-in (make-wire)))
      (if (null? an)
        'ok
        (begin
          (set-signal! c-in value-of-c)
          (full-adder a b c-in s c)
          (iter (cdr an) (cdr bn) (cdr sn)
                (get-signal c))))))
    (iter an bn sn (get-signal c)))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (or-gate a1 a2 output)
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