#lang sicp

;; work5.4
;; b)
(controller
 expt-loop
   (assign n (op read))
   (assign b (op read))
   (assign product (const 1))
 test-expt
   (test (op =) (reg n) (const 0))
   (branch (label expt-done))
   (assign product (op *) (reg b) (reg product))
   (assign n (op -) (reg n) (const 1))
   (goto (label test-expt))
 expt-done
   (perform (op print) (reg product))
   (goto (label expt-loop)))

;; a)
(controller
  (assign continue (label expt-done))
 expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-expt))
   (goto (label expt-loop))
 after-expt
   (restore n)
   (restore continue)
   (assign val (op *) (reg b) (reg val))
   (goto (reg continue))
 base-case
   (assign val (const 1))
   (goto (reg continue))
 expt-done)