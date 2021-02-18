#lang sicp

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-list)
      (define (iter key-list table)
        (let ((record (assoc (car key-list) (cdr table))))
          (if record
              (if (null? (cdr key-list))
                  (cdr record)
                  (iter (cdr key-list) record))
              #f)))
        (iter key-list local-table))
    (define (insert! key-list value)
      (define (iter key-list table)
        (let ((record (assoc (car key-list) (cdr table))))
          (if record
              (if (null? (cdr key-list))
                  (set-cdr! record value)
                  (iter (cdr key-list) record))
              (if (null? (cdr key-list))
                  (set-cdr! table
                            (cons (cons (car key-list) value)
                                  (cdr table)))
                  (begin (set-cdr!
                          table
                          (cons (cons (car key-list) '())
                                (cdr table)))
                         (iter (cdr key-list) (cadr table)))))))
        (begin (iter key-list local-table)
               'ok))    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t (make-table equal?))