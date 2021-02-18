#lang sicp
;未完成，有待补全

(define (tags->set t)
  (union-set (list (car t)) (cdr t)))

(define (coercion-types s type)
  (map (lambda (x)
         (if (equal? (type-tag x) type)
             x
             ((get-coercion (type-tag x) type) x)))
       s))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((tags-set (tags->set type-tags)))
            (if (= (length (tags-set)) 1)
                (error "No method for these types"
                                (list op type-tags))
                (let ((type (coercion-types args
                                            (car tags-set))))
                  (if type
                      (apply-generic op type)))))))))
                

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) '())
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else
         (cons (car set1)
               (union-set (cdr set1) set2)))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))