#lang sicp

;; a)
ev-application
  (assign unev (op operands) (reg exp))  ; adjust the order
  (assign exp (op operator) (reg exp))  ; adjust the order
  (test (op symbol?) (reg exp))  ; add
  (branch (label ev-apply-symbol-operator))  ; add
  (save continue)
  (save env)
  (save unev)
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-symbol-operator  ; add
  (assign proc (op lookup-variable-value) (reg exp) (reg env))
  (branch (label ev-appl-common-operator))
ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign proc (reg val))
ev-appl-common-operator  ; add
  (assign argl (op empty-arglist))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))