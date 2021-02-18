#lang sicp

;; mutators
(define (front-insert-deque! dq item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? dq)
           (set-front-ptr! dq new-pair)
           (set-rear-ptr! dq new-pair)
           (front-ptr dq))
          (else
           (set-cdr! (cdr new-pair) (front-ptr dq))
           (set-car! (cdr (front-ptr dq)) new-pair)
           (set-front-ptr! dq new-pair)
           (front-ptr dq)))))

(define (rear-insert-deque! dq item)
  (let ((new-pair (cons item (cons '() '()))))
    (cond ((empty-deque? dq)
           (set-front-ptr! dq new-pair)
           (set-rear-ptr! dq new-pair)
           (front-ptr dq))
          (else
           (set-car! (cdr new-pair) (rear-ptr dq))
           (set-cdr! (cdr (rear-ptr dq)) new-pair)
           (set-rear-ptr! dq new-pair)
           (front-ptr dq)))))

(define (front-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error "DELETE! called with an empty deque"
                (front-ptr dq)))
        (else
         (set-front-ptr! dq (cddr (front-ptr dq)))
         (front-ptr dq))))

(define (rear-delete-deque! dq)
  (cond ((empty-deque? dq)
         (error "DELETE! called with an empty deque"
                (front-ptr dq)))
        (else
         (set-rear-ptr! dq (cadr (rear-ptr dq)))
         (rear-ptr dq)))) 

;; selectors
(define (front-deque dq)
  (if (empty-deque? dq)
      (error "FRONT called with an empty deque" dq)
      (car (front-ptr dq))))

(define (rear-deque dq)
  (if (empty-deque? dq)
      (error "FRONT called with an empty deque" dq)
      (car (rear-ptr dq))))

;; constructors
(define (make-deque) (cons '() '()))
(define (empty-deque? dq)
  (or (null? (front-ptr dq))
      (null? (rear-ptr dq))))

(define (front-ptr dq) (car dq))
(define (rear-ptr dq) (cdr dq))
(define (set-front-ptr! dq item) (set-car! dq item))
(define (set-rear-ptr! dq item) (set-cdr! dq item)) 