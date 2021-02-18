#lang sicp
;;已初步完成，有待之后调试

(define (install-polynomial-package)
  ;; imported procedures from sparse and dense packages
  (define (make-from-sparse-poly variable sparse-term-list)
    ((get 'make 'sparse) variable sparse-term-list))
  (define (make-from-dense-poly variable dense-term-list)
   ((get 'make 'dense) variable dense-term-list))

  ;; internal procedures
  ;;(define (add-poly p1 p2) ... )
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-dense-poly
         (variable p1)
         (add-dense-terms (dense-term-list p1)
                          (dense-term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  
  (define (add-dense-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (add-terms L1
                               (adjoin-term
                                (make-term (length L2) 0) L2)))
                   ((< (order t1) (order t2))
                    (add-terms
                     (adjoin-term (make-term (length L1) 0)
                                  L1)
                     L2))
                   (else
                    (map (lambda (x y) (+ x y)) L1 L2)))))))

  (define (add-sparse-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  ;;(define (mul-poly p1 p2) ... )
  (define (the-empty-termlist) '())
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-sparse-poly
         (variable p1)
         (mul-sparse-terms (sparse-term-list p1)
                           (sparse-term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))

  (define (mul-sparse-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-sparse-terms
         (mul-term-by-all-terms (first-term L1) L2)
         (mul-terms (rest-terms L1) L2))))
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  ;;(define (sub-poly p1 p2) ... )
  (define (nega-terms t)
    (if (empty-termlist? t)
        the-empty-termlist
        (let ((tf (first-term t)) (tr (rest-terms t)))
          (adjoin-term
           (make-term (order tf) (negative (coeff tf)))
           (nega-terms tr)))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'the-empty-termlist 'polynomial the-empty-termlist)
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make-from-sparse-poly 'polynomial
       (lambda (var terms)
         (tag (make-from-sparse-poly var terms))))
  (put 'make-from-dense-poly 'polynomial
       (lambda (var terms)
         (tag (make-from-dense-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (x) (empty-termlist? (sparse-term-list x))))
  (put 'negative 'polynomial
       (lambda (x)
         (make-polynomial (variable x)
                          (nega-terms (sparse-term-list x)))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (negative p2)))))
  'done)

(define (dense-term-list p)
  (apply-generic 'dense-term-list p))

(define (sparse-term-list p)
  (apply-generic 'sparse-term-list p))

(define (adjoin-term x term-list)
  (apply-generic 'adjoin-term x term-list))
(define (first-term term-list)
  (apply-generic 'first-term term-list))
(define (rest-terms term-list)
  (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list)
  (apply-generic 'empty-termlist? term-list))
(define (make-term order coeff)
  (apply-generic 'make-term order coeff))
(define (order term)
  (apply-generic 'order term))
(define (coeff term)
  (apply-generic 'coeff term))

(define (install-dense-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-dense-poly variable dense-term-list)
    (cons variable dense-term-list))
  (define (variable p) (car p))
  (define (dense-term-list p) (cdr p))
  (define (sparse-term-list p)
    (define (dense->sparse t)
      (adjoin-term (first-term t)
                   (dense->sparse (rest-terms t))))
    (filter (lambda (x) (not (=zero? (coeff x))))
            (dense->sparse (dense-term-list p))))

  ;; representation of terms and term lists
  (define (the-empty-termlist) '())
  (define (first-term term-list)
    (make-term (- (length (term-list)) 1) (car term-list)))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term tl)
    (if (=zero? (coeff term))
        tl
        (let ((tf (first-term tl)) (tr (rest-terms tl)))
          (cond ((or (empty-termlist? tl)
                     (> (order term) (order tf)))
                 (adjoin-term term (cons 0 tl)))
                ((= (order term) (order tf))
                 (cons (+ (coeff term) (coeff tf)) tr))
                ((< (order term) (order tf))
                 (cons (coeff tf)
                       (adjoin-term term (cons 0 tr))))))))
  
  ;; interface to the rest of the system
  (define (tag d) (attach-tag 'dense d))
  (put 'make 'dense
       (lambda (var terms) (tag (make-dense-poly var terms))))
  (put 'variable '(dense) variable)
  (put 'dense-term-list '(dense)
       (lambda (d) (tag (dense-term-list d))))
  (put 'sparse-term-list '(dense)
       (lambda (d) (attach-tag 'sparse (sparse-term-list d))))
  (put 'adjoin-term '(dense dense)
       (lambda (x y) (tag (adjoin-term x y))))
  (put 'adjoin-term '(sparse dense)
       (lambda (x y) (tag (adjoin-term x y))))
  (put 'the-empty-termlist 'dense the-empty-termlist)
  (put 'frist-term '(dense)
       (lambda (d) (tag (first-term d))))
  (put 'rest-terms '(dense)
       (lambda (d) (tag (rest-terms d))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'make-term '(dense)
       (lambda (x y) (tag (make-term x y))))
  (put 'order '(dense) order)
  (put 'coeff '(dense) coeff)
  'done)


(define (install-sparse-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-sparse-poly variable sparse-term-list)
    (cons variable sparse-term-list))
  (define (variable p) (car p))
  (define (sparse-term-list p) (cdr p))
  (define (dense-term-list p)
    (define (sparse->dense t)
      (let ((t1 (first-term t))
            (t2 (first-term (rest-terms t))))
        (if (= (- (order t1) (order t2)) 1)
            (cons t1 (sparse->dense (rest-terms t)))
            (cons t1 (cons (make-term (- (order t1) 1) 0)
                           (sparse->dense (rest-terms t)))))))
    (map (lambda (x) (car x))
         (sparse->dense (sparse-term-list p))))

  ;; representation of terms and term lists
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term sparse-term-list)
    (if (=zero? (coeff term))
        sparse-term-list
        (cons term sparse-term-list)))
  
  ;; interface to the rest of the system
  (define (tag s) (attach-tag 'sparse s))
  (put 'make 'sparse
       (lambda (var terms) (tag (make-sparse-poly var terms))))
  (put 'variable '(sparse) variable)
  (put 'sparse-term-list '(sparse)
       (lambda (s) (tag (sparse-term-list s))))
  (put 'dense-term-list '(sparse)
       (lambda (s) (attach-tag 'dense (dense-term-list s))))
  (put 'adjoin-term '(sparse sparse)
       (lambda (x y) (tag (adjoin-term x y))))
  (put 'adjoin-term '(dense sparse)
       (lambda (x y) (tag (adjoin-term x y))))
  (put 'the-empty-termlist 'sparse the-empty-termlist)
  (put 'frist-term '(sparse)
       (lambda (s) (tag (first-term s))))
  (put 'rest-terms '(sparse)
       (lambda (s) (tag (rest-terms s))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'make-term '(sparse)
       (lambda (x y) (tag (make-term x y))))
  (put 'order '(sparse) order)
  (put 'coeff '(sparse) coeff)
  'done)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else
         (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))