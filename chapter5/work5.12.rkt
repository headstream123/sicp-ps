#lang sicp

;; work5.12
;  经过验证,可以运行,关于register的value来源,出于一(tou)些(lan)考虑,使用了
;  和分类代码同样的程序,所以在显示上有些欠缺,不过也还算清晰易懂
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (all-instructions '()))  ; add
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                ;;**next for monitored stack (as in section 5.2.4)
                ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (sort-instructions insts)  ; add
        (let ((table '()))
          (define (new-sort? i t)
            (cond ((null? t) true)
                  ((eq? (car i) (caar t)) false)
                  (else (new-sort? i (cdr t)))))
          (define (add-if-possible i t)
            (cond ((null? t) (error "new instruction" i))
                  ((eq? (car i) (caar t))
                   (set-cdr! (car t) (cons i (cdar t))))
                  (else (add-if-possible i (cdr t)))))
          (for-each (lambda (inst)
                      (if (new-sort? inst table)
                          (set! table (cons (list (car inst) inst)
                                            table))
                          (add-if-possible inst table)))
                    insts)
          table))
      (define (goto-registers)  ; add
        (let ((insts (assoc 'goto all-instructions)))
          (if insts
              (map (lambda (reg) (cadadr reg))
                   (unique
                    (filter (lambda (inst) (eq? (caadr inst) 'reg))
                            (cdr insts))))
              (error "Not goto instructions"))))
      (define (stack-registers)  ; add
        (let ((si (assoc 'save all-instructions))
              (ri (assoc 'restore all-instructions)))
          (if (and si ri)
              (unique
               (map (lambda (inst) (cadr inst))
                    (append (cdr si) (cdr ri))))
              (error "Not stack instructions"))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)
                 (set! all-instructions  ; change
                       (unique
                        (map (lambda (inst) (car inst)) seq)))))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'instructions)
               (set! all-instructions
                     (sort-instructions all-instructions))
               all-instructions);add
              ((eq? message 'goto-registers)
               (goto-registers)) ; add
              ((eq? message 'stack-registers)
               (stack-registers)) ; add
              ((eq? message 'registers-value-from-list)
               (let ((insts
                      (cdr (assoc 'assign all-instructions))))
                 (sort-instructions (map cdr insts)))) ; add
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (unique sequence)
  (define (iter s result)
    (cond ((null? s) (reverse result))
          ((included? (car s) result)
           (iter (cdr s) result))
          (else (iter (cdr s) (cons (car s) result)))))
  (iter sequence '()))

(define (included? item sequence)
  (cond ((null? sequence) false)
        ((equal? item (car sequence)) true)
        (else (included? item (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

