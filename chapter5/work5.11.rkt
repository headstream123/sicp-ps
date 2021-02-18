#lang sicp

;; work5.11
;; c)
(define (make-save inst machine stack pc)
  (let ((name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine name))
          (s (assoc name stack)))
      (lambda ()
        (push (cadr s) (get-contents reg))
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine name))
          (s (assoc name stack)))
      (lambda ()
        (set-contents! reg (pop (cadr s)))
        (advance-pc pc)))))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack-table '())  ; add
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda ()
                         (for-each (lambda (s)
                                     ((cadr s) 'initialize))
                                   (stack-table))))))  ; change
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (begin
              (set! stack-table  ; add
                    (cons (list name make-stack) stack-table))
              (set! register-table
                    (cons (list name (make-register name))
                          register-table))))
        'register-allocated)
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack-table) ; change
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message)))
        )dispatch)))

;; b)
(define (make-save inst machine stack pc)
  (let ((name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine name)))  ; change
      (lambda ()
        (push stack (cons name (get-contents reg)))  ; change
        (advance-pc pc)))))

(define (make-restore inst machine stack pc)
  (let ((target-name (stack-inst-reg-name inst)))
    (let ((reg (get-register machine target-name))
          (restore (pop stack)))  ; change
      (lambda ()
        (if (equal? (car restore) target-name)  ;change
            (begin (set-contents! reg (cdr restore))
                   (advance-pc pc))
            (begin (push stack restore)
                   (error "different register" target-name)))))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))