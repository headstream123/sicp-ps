#lang sicp

;; work5.34
; 总体来说迭代版本的fact在apply iter时,直接goto val, 而递归版本则assign
; continue label, 因为后者需要返回, so stack就一直累积, 直到最后restore

((env) (val)

 ; construct the procedure(fact)       
 ((assign val (op make-compiled-procedure) (label entry1)
                                           (reg env))
  (goto (label after-lambda2))
 entry1 ; calls to factorial will enter here
 
  ; extend env, update the values of product and counter 
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment)
              (const (n)) (reg argl) (reg env))
  ; construct the procedure (iter)
  (assign val (op make-compiled-procedure) (label entry3)
                                           (reg env))
  (goto (label after-lambda4))
 entry3 ; calls to iter will enter here
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter))
                                      (reg argl) (reg env))
  ; begin actual procedure body(iter)
  (save continue)
  (save env)

  ; compute (> counter n)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
 compiled-branch9
  (assign continue (label after-call10))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
 primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call10 ; val now contains result of (> counter n)
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch6))
 true-branch5 ; return product
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
 false-branch6
 ; compute and return (iter (* counter product) (+ counter 1))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc) ; save iter procedure
  (save env)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
 compiled-branch15
  (assign continue (label after-call16))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
 primitive-branch14
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call16 ; val now contains result of (+ counter 1)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl) ; save result of (+ counter 1)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
 compiled-branch12
  (assign continue (label after-call13))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
 primitive-branch11
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
 after-call13 ; val now contains result of (* counter product)
  (restore argl) ; restore result of (+ counter 1)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ; restore iter procedure
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
 compiled-branch18 ; goto entry3 and iteration
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
 primitive-branch17
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
 after-call19
 after-if7
 after-lambda4
  ; assign the procedure to the variable iter and bind arguments
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch20))
 compiled-branch21
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
 primitive-branch20
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
 after-call22
 after-lambda2
   ; assign the procedure to the variable factorial
  (perform (op define-variable!) (const factorial) (reg val)
                                 (reg env))
  (assign val (const ok))))