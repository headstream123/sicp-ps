#lang sicp
(#%require sicp-pict)

;(paint f)

(define (square-limit-new painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (corner-split-simple painter n)
  (let ((top-left (up-split painter (- n 1)))
        (bottom-right (right-split painter (- n 1)))
        (corner (corner-split painter (- n 1))))
    (beside (below painter top-left)
            (below bottom-right corner))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (split out in)
  (lambda (p n)
    (if (= n 0)
      p
      (let ((smaller ((split out in) p (- n 1))))
        (out p (in smaller smaller))))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))