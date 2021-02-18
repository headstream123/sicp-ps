#lang sicp

;; work4.75
;  b)
;  这道题要求找出只有一个上司的人,一种方法是定义rule supervisor,增加上司的
;  上司也是上司这一概念,之后就使用下面语句查询
;  另一种方法也如下,但不需定义什么,实际上每个人本来就"只有一个上司"233
(and (supervisor ?x ?y) (unique (supervisor ?x ?anyone)))

;  a)
(define (unique-query exps) (car exps))

(define (uniquely-asserted contents frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((results (qeval (unique-query contents)
                           (singleton-stream frame))))
     (if (singleton-stream? results)
         results
         the-empty-stream)))
   frame-stream))

(define (singleton-stream? s)
  (and (not (stream-null? s))
       (stream-null? (stream-cdr s)))) 