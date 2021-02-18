#lang sicp

;; work4.50
;  随机程序使用了之前蒙特卡洛模拟时的程序
;  随机选取可能性后4.49hacker的程序就可以从very boring变成boring,好一些
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next (shuffle-list cprocs)))))

(define (shuffle-list list)
  (let ((n (length list)))
    (define (roll-list order result)
      (let ((dice (random-in-range 1 (+ n 1))))
        (cond ((= (length result) n)
               (reverse result))
              ((not (memq dice order))
               (roll-list (cons dice order)
                          (cons (draw dice list) result)))
              (else (roll-list order result)))))
    (roll-list '() '())))

(define (draw n list)
  (if (= n 1)
      (car list)
      (draw (- n 1) (cdr list))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))