#lang sicp

;; work4.76
;  此题完全使用了书中的unify程序对两个frame进行合并,对于一般的and都可以使用
;  但是试验4.69中的程序时进入了无限循环,so对于这种递归程序以下and无法正确执行
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (let ((first (first-conjunct conjuncts))
            (rest (rest-conjuncts conjuncts)))
        (cond ((empty-conjunction? rest)
               (qeval first frame-stream))
              (else (conjoin
                     (rest-conjuncts rest)
                     (compatible-frame
                      (qeval first frame-stream)
                      (qeval (first-conjunct rest)
                             frame-stream))))))))

(define (compatible-frame first-stream second-stream)
  (stream-flatmap
   (lambda (first)
     (stream-flatmap
      (lambda (second)
        (let ((result (match-a-frame second first)))
          (if (eq? result 'failed)
              the-empty-stream
              (singleton-stream result))))
      second-stream))
   first-stream))

(define (match-a-frame f1 f2)
  (define (iter target frame)
    (cond ((eq? frame 'failed) 'failed)
          ((null? target) frame)
          (else
           (let ((var (binding-variable (car target)))
                 (val (binding-value (car target))))
             (iter (cdr target)
                   (unify-match var val frame))))))
  (cond ((stream-null? f1) 'failed)
        ((stream-null? f2) 'failed)
        (else (iter f1 f2))))


;; Auxiliary
(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))