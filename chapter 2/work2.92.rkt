#lang sicp
;粗暴完成(算是)。思路：以字母表顺序作为排序依据，所有poly都变换到此种
;“标椎形式”，若p1 p2主元相等，则直接相加。否则找出p1 p2中>的那个，
;以此为主元运算，采用var-up算法。其中涉及到多项式与整数等混合运算，方法一
;是将各种混合运算put进各种包，方法二是设置polynomial为complex的上级
;设置一个raise方法。以下采用了简单粗暴的方法二。塔策略确实不适合多项式运算。

(put 'raise 'complex
     (lambda (x) (var-up 'a x)))

(define (var-up var p)
  (make-poly var (adjoin-term (make-term 0 p)
                              (the-empty-termlist))))

(define (higher? v1 v2)
  (string>? (symbol->string v1) (symbol->string v2)))

(define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (if (higher? (variable p1) (variable p2))
            (add-poly p1 (var-up (variable p1) p2))
            (add-poly (var-up (variable p2) p1) p2))))

(define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (if (higher? (variable p1) (variable p2))
            (mul-poly p1 (var-up (variable p1) p2))
            (mul-poly (var-up (variable p2) p1) p2))))