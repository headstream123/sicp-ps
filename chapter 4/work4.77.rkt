#lang sicp

;; work4.77
;  not和lisp-value都是过滤器,so它们和or合用(没有前置),或者单独用,
;  本来就不能工作,因此只需改变and,使其在求值时检测not或lisp-value所需变量
;  是否均已绑定,这里考量的是,通常绑定的变量各个frame都是一样的,只是value不同
;  当然这一点很不严谨,但是出于实现(tou lan)上的考虑,这里仅在frame-stream中
;  抽出一个frame进行判定,进行了一些简单测试,都可以正常工作,当然想让它不能工作
;  也很简单就是了
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (let ((first (first-conjunct conjuncts))
            (rest (rest-conjuncts conjuncts)))
        (if (or (eq? (type first) 'not)
                (eq? (type first) 'lisp-value))
            (if (all-bind? (cadr first)
                           (get-frame frame-stream))
                (conjoin rest (qeval first frame-stream))
                (conjoin (append (list (first-conjunct rest)
                                       first)
                                 (rest-conjuncts rest))
                         frame-stream))
            (conjoin rest (qeval first frame-stream))))))

(define (get-frame frame-stream)
  (if (stream-null? frame-stream)
      the-empty-stream
      (if (stream-null? (stream-car frame-stream))
          (get-frame (stream-cdr frame-stream))
          (stream-car frame-stream))))

(define (all-bind? contents frame)
  (cond ((stream-null? frame) false)
        ((null? contents) true)
        ((and (var? (car contents))
              (bind? (car contents) frame))
         (all-bind? (cdr contents) frame))
        ((constant-symbol? (car contents))
         (all-bind? (cdr contents) frame))
        ((and (pair? (car contents))
              (all-bind? (car contents) frame))
         (all-bind? (cdr contents) frame))
        (else false)))

(define (bind? var frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (if (var? (binding-value binding))
            (bind? (binding-value binding) frame)
            true)
        false)))