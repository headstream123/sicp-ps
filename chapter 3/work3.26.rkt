#lang sicp

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    
    (define (more-than? a b)
      (cond ((and (number? a) (number? b)) (> a b))
            ((and (symbol? a) (symbol? b))
             (string>? (symbol->string a) (symbol->string b)))
            (else (error "Unable to compare"))))
    
    (define (lookup key)
      (define (f key table)
        (cond ((null? table) #f)
              ((same-key? key (get-key table))
               (get-value table))
              ((more-than? key (get-key table))
               (f key (right-branch table)))
              (else
               (f key (left-branch table)))))
      (f key (cdr local-table)))
    
    (define (insert! key value)
       (define (adjoin-set x set)
         (cond ((null? set) (make-tree x '() '()))
               ((same-key? (car x) (car (entry set)))
                (begin (set-cdr! (entry set) (cdr x)) set))
               ((more-than? (car x) (car (entry set)))
                (make-tree (entry set)
                           (left-branch set)
                           (adjoin-set x (right-branch set))))
               (else
                (make-tree (entry set)
                           (adjoin-set x (left-branch set))
                           (right-branch set)))))
      (if (null? (cdr local-table))
          (set-cdr! local-table
                    (make-tree (cons key value) '() '()))
          (set-cdr! local-table
                    (adjoin-set (cons key value)
                                (cdr local-table))))
      'ok)
      
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
(define (get-key tree) (car (entry tree)))
(define (get-value tree) (cdr (entry tree)))

(define t (make-table equal?))
(define get (t 'lookup-proc))
(define put (t 'insert-proc!))