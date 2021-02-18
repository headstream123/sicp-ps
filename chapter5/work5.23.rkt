#lang sicp

;; 'error' of '(perform (op error))' will be defined,
;; it's a procedure that print some error informations

;; work5.24
ev-cond-direct
  (assign unev (op cond-clauses) (reg exp))
ev-cond
  (test (op null?) (reg unev))
  (branch (label ev-cond-error))
  (save unev)
  (save env)
  (save continue)
  (assign continue (label ev-cond-decide))
  (assign unev (op car) (reg unev))
  (test (op cond-else-clause?) (reg unev))
  (branch (label ev-cond-last-exp))
  (assign exp (op cond-predicate) (reg unev))
  (goto (label eval-dispatch))
ev-cond-decide
  (restore env)
  (restore unev)
  (test (op true?) (reg val))
  (branch (label ev-cond-consequent))
  (assign unev (op cdr) (reg unev))
  (goto (label ev-cond))
ev-cond-consequent
  (restore continue)
  (assign unev (op car) (reg unev))
  (assign unev (op cond-actions) (reg unev))
  (goto (label ev-sequence))
ev-cond-last-exp
  (restore unev)
  (test (op last-exp?) (reg unev))
  (branch (label ev-cond-consequent))
ev-cond-error
  (perform (op error))
    
;; work5.23
eval-dispatch
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  (test (op let?) (reg exp))
  (branch (label ev-let))

ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto eval-dispatch)

ev-let
  (assign exp (op let->combination) (reg exp))
  (goto eval-dispatch)