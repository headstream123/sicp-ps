#lang sicp

;; mutators
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           (front-ptr queue))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           (front-ptr queue))))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue"
                (front-ptr (queue))))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         (front-ptr queue)))) 

;; selectors
(define (empty-queue? q)
  (null? (front-ptr q)))

(define (front-queue q)
  (if (empty-queue? q)
      (error "FRONT called with an empty queue" (front-ptr q))
      (car (front-ptr q))))

;; constructors
(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)))
    dispatch))

(define (front-ptr q) (q 'front-ptr))
(define (rear-ptr q) (q 'rear-ptr))
(define (set-front-ptr! q item)
  ((q 'set-front-ptr!) item))
(define (set-rear-ptr! q item)
  ((q 'set-rear-ptr!) item))