#lang sicp

;; 5.15-5.19
;  5.19中print和cancel都是简单版本,因为考虑到不同的label和n指向同一个句子的
;  情况在这种简单机器中基本不会出现. 还有使用句子作为key, 而不是用set!计数,是
;  出于设置断点时应该是看着控制码来设置,实际运行时会goto,计数的话不能保证是同一句
;  指令.

;; Register-Machine Simulator
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on false)) ; 5.18
    (define (dispatch message)
      (define (print-informations new)
        (newline)
        (display (list 'register-name  '= name
                       'old-contents   '= contents
                       'new-contents   '= new)))
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) ; 5.18
             (lambda (value)
               (if trace-on
                   (begin (print-informations value)
                          (set! contents value))
                   (set! contents value))))
            ((eq? message 'trace-on) (set! trace-on true)) ;5.18
            ((eq? message 'trace-off) (set! trace-on false)) ;5.18
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;**monitored version from section 5.2.4
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))    
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (labels '())  ; work5.19
        (breakpoints (list '*break*))  ; work5.19
        (trace-flag true)  ; work5.16
        (number-insts 0))  ; work5.15
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                ;;**next for monitored stack (as in section 5.2.4)
                ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      
      (define (set-breakpoint label n) ; 5.19
        (let ((inst (lookup-inst label n)))
          (if inst
              (insert! inst (cons label n) breakpoints)
              (error "No instruction" label n))))
      (define (lookup-inst label n) ; 5.19
        (define (iter n insts)
          (if (= n 1)
              (instruction-text (car insts))
              (iter (- n 1) (cdr insts))))
        (let ((record (assoc label labels)))
          (if record
              (iter n (cdr record))
              false)))
      (define (print-breakpoint table) ; 5.19
        (begin (newline)
               (display (list 'label  '= (car table)
                              'offset '= (cdr table)))))
      (define (cancel-breakpoint label n) ; 5.19
        (let ((key (lookup-inst label n)))
          (let ((record (assoc key (cdr breakpoints))))
            (if record
                (begin
                  (set-cdr! breakpoints
                  (filter (lambda (x) (not (equal? (car x) key)))
                          (cdr breakpoints)))
                  'done)
                (error "Don't exist the breakpoint" label n)))))
      (define (print-trace inst); 5.16-5.17
        (if trace-flag
            (if (symbol? (instruction-label inst))
                (begin (newline)
                       (display (instruction-label inst))
                       (newline)
                       (display (instruction-text inst)))
                (begin (newline)
                       (display (instruction-text inst))))))
      (define (register-trace name switch) ; 5.18
        (let ((register (lookup-register name)))
          (if register
              (if (eq? 'trace-on switch)
                  (register 'trace-on)
                  (register 'trace-off))
              (error "Unknown register" name))))
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
                (print-trace (car insts)) ;5.16-5.17
                ;; (p)才能执行程序,so下面多了一对括号
                ((instruction-execution-proc (car insts)))
                (set! number-insts (+ number-insts 1)) ; work5.15
                (if (and (not (null? (cdr insts)))
                         (lookup (instruction-text (cadr insts))
                                 breakpoints))
                    (print-breakpoint
                     (lookup (instruction-text (cadr insts))
                             breakpoints))
                    (execute)))))) ; 5.19
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
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'number-instructions)
               (newline)
               (display (list 'total-instructions '= number-insts))
               (newline)
               (set! number-insts 0))  ; work5.15
              ((eq? message 'trace-on) (set! trace-flag true));5.16
              ((eq? message 'trace-off) (set! trace-flag false))
              ((eq? message 'register-trace-on) ; 5.18
               (lambda (name) (register-trace name 'trace-on)))
              ((eq? message 'register-trace-off) ; 5.18
               (lambda (name) (register-trace name 'trace-off)))
              ((eq? message 'set-labels!)
               (lambda (seq) (set! labels seq))) ; 5.19
              ((eq? message 'set-breakpoint!)
               (lambda (label n) (set-breakpoint label n))) ; 5.19
              ((eq? message 'proceed-machine) (execute)) ; 5.19
              ((eq? message 'cancel-breakpoint)
               (lambda (label n) (cancel-breakpoint label n)));5.19
              ((eq? message 'cancel-all-breakpoints)
               (set-cdr! breakpoints '())
               'done) ; 5.19
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (register-trace-on machine reg-name) ; 5.18
  ((machine 'register-trace-on) reg-name))

(define (register-trace-off machine reg-name) ; 5.18
  ((machine 'register-trace-off) reg-name))

(define (set-breakpoint machine label n)  ; 5.19
  ((machine 'set-breakpoint!) label n))

(define (cancel-breakpoint machine label n)  ; 5.19
  ((machine 'cancel-breakpoint) label n))

(define (cancel-all-breakpoints machine)  ; 5.19
  (machine 'cancel-all-breakpoints))

(define (process-machine machine)  ; 5.19
  (machine 'process-machine))  

(define (assemble controller-text machine)
  (extract-labels controller-text '()  ; 5.17
    (lambda (insts labels)
      ((machine 'set-labels!) labels)  ; 5.19
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text label receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (update-label! label (car text)) ; 5.17
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                        (cons (make-label-entry next-inst
                                                insts)
                              labels))
               (receive (cons (make-instruction label next-inst)
                              insts)  ; 5.17
                        labels)))))))

(define (update-label! old-label inst)  ; 5.17
  (cond ((symbol? inst) inst)
        ((not (null? old-label)) '())
        (else old-label)))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc! 
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction label text)  ; 5.17
  (list label text '())) 

(define (instruction-label inst)  ; 5.17
  (car inst))

(define (instruction-text inst)  ; 5.17
  (cadr inst))

(define (instruction-execution-proc inst)
  (caddr inst)) ; 5.17

(define (set-instruction-execution-proc! inst proc)
  (set-car! (cddr inst) proc))  ; 5.17

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))


(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE"
                     inst))))


(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                ; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))


(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))


(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))

(define (register-exp-reg exp) (cadr exp))

(define (constant-exp? exp) (tagged-list? exp 'const))

(define (constant-exp-value exp) (cadr exp))

(define (label-exp? exp) (tagged-list? exp 'label))

(define (label-exp-label exp) (cadr exp))


(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))


(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;; from 4.1
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; from 3.3
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table) 
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record (cons value (cdr record)))  ; changed
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

;; from 2.2
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;; complicate version
#|
(define (print-breakpoint table) ; 5.19
  (if (pair? (car table))
      (for-each (lambda (x)
                  (newline)
                  (display (list 'label  '= (car x)
                                 'offset '= (cdr x)))) table)
      (begin (newline)
             (display (list 'label  '= (car table)
                                    'offset '= (cdr table))))))
(define (cancel-breakpoint label n) ; 5.19
  (let ((key (lookup-inst label n)))
    (let ((record (assoc key (cdr breakpoints))))
      (if record
          (let ((rest (filter (lambda (x)
                                (not (equal? x (cons label n))))
                              (cdr record))))
            (if (null? rest)
                (set-cdr! breakpoints
                          (filter (lambda (x)
                                    (not (equal? (car x) key)))
                                  (cdr breakpoints)))
                (set-cdr! record rest)))
          (error "Don't exist the breakpoint" label n)))))
|#

'(REGISTER SIMULATOR LOADED)

(define fib-machine
  (make-machine
   '(continue n val)
   (list (list '< <) (list '- -) (list '+ +)
         (list 'print display))
   
   '(
       (assign continue (label fib-done))
     fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label immediate-answer))
       (save continue)
       (assign continue (label afterfib-n-1))
       (save n)                           
       (assign n (op -) (reg n) (const 1))
       (goto (label fib-loop))            
     afterfib-n-1                         
       (restore n)
       (assign n (op -) (reg n) (const 2))
       (assign continue (label afterfib-n-2))
       (save val)                         
       (goto (label fib-loop))
     afterfib-n-2                         
       (assign n (reg val))               
       (restore val)                      
       (restore continue)
       (assign val                        
               (op +) (reg val) (reg n)) 
       (goto (reg continue))              
     immediate-answer
       (assign val (reg n))              
       (goto (reg continue))
     fib-done
       (perform (op print) (reg val)))))

(define fact-machine
  (make-machine
   '(continue n val)
   (list (list '= =) (list '* *) (list '- -) (list 'read read))
   '(fact-loop
      (perform (op initialize-stack))
      (assign n (op read))
      (assign continue (label fact-done))    
     test-n
      (test (op =) (reg n) (const 1))
      (branch (label base-case))
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label test-n))
     after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))   
      (goto (reg continue))                   
     base-case
      (assign val (const 1))                  
      (goto (reg continue))                   
     fact-done
      (perform (op print-stack-statistics))
      (goto (label fact-loop)))))