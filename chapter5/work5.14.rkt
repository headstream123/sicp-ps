#lang sicp

;; work5.14
;  n和total-pushes, maximum-depth的线性关系公式为p = 2n - 2
;  ps: 由于没有pop操作因此, 公式是一样的
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