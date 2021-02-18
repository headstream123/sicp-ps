#lang sicp

(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (let ((merge (make-code-tree (car set) (cadr set))))
        (successive-merge (adjoin-set merge (cddr set))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (encode-symbol message tree)
  (define (es m t)
    (let ((left (left-branch t))
          (right (right-branch t)))
      (cond ((leaf? t) '())
            ((element-of-set? m (symbols left))
             (cons 0 (es m left)))
            ((element-of-set? m (symbols right))
             (cons 1 (es m right))))))
  (if (not (element-of-set? message (symbols tree)))
      (error "Not in the tree")
      (es message tree)))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define rock
  (generate-huffman-tree
   '((a 1) (na 16) (boom 1) (Sha 3) (Get 2)
           (yip 9) (job 2) (Wah 1))))