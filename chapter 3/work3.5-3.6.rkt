#lang sicp

;work3.6		
(define rand
  (let ((random 3))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin (set! random (rand-update random))
                    random))
            ((eq? m 'reset)
             (lambda (x) (set! random x)))
            (else
             (error "Unknown request -- RAND" m))))))

(define (rand-update x)
  (let ((a 27) (b 26) (m 127))
    (modulo (+ (* a x) b) m)))

;work3.5
;其实直接用正方形就可以，参数直接就是x y，prop*4，random取值+1 -1即可
(define (estimate-integral p x1 x2 y1 y2 trials)
  (let ((prop (monte-carlo trials (p x1 x2 y1 y2))))
    (* prop (- y2 y1) (- x2 x1))))

(define (p-test x1 x2 y1 y2)
  (lambda ()
    (<= (+ (square (- (random-in-range x1 x2) (/ (+ x1 x2) 2)))
           (square (- (random-in-range y1 y2) (/ (+ y1 y2) 2))))
        1)))

(define (square x) (* x x))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random (exact->inexact range)))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials 1.0))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
