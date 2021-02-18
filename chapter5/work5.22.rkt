#lang sicp

;; work5.22
;; b)
(define append!-machine
  (make-machine
   '(temp t1 x y)
   (list (list 'null? null?) (list 'set-cdr! set-cdr!)
         (list 'cdr cdr) (list 'print display))
'(
   (assign temp (reg x))
 append-loop
   (assign t1 (op cdr) (reg temp))
   (test (op null?) (reg t1))
   (branch (label null))
   (assign temp (op cdr) (reg temp))
   (goto (label append-loop))
 null
   (perform (op set-cdr!) (reg temp) (reg y))
   (goto (label append-done))
 append-done
   (perform (op print) (reg x)))))

;; a)
(define append-machine
  (make-machine
   '(continue x y)
   (list (list 'null? null?) (list 'cons cons) (list 'car car)
         (list 'cdr cdr) (list 'print display))
'(
   (assign continue (label append-done))
 append-loop
   (test (op null?) (reg x))
   (branch (label append-null))
   (save continue)
   (save x)
   (assign continue (label append-null))
   (assign x (op cdr) (reg x))
   (goto (label append-loop))
 append-null
   (restore x)
   (assign x (op car) (reg x))
   (assign y (op cons) (reg x) (reg y))
   (restore continue)
   (goto (reg continue))
 append-done
   (perform (op print) (reg y)))))

#|(define append-machine
  (make-machine
   '(continue x y)
   (list (list 'null? null?) (list 'cons cons) (list 'car car)
         (list 'cdr cdr) (list 'print display))
'(
   (assign continue (label append-done))
 append-loop
   (test (op null?) (reg x))
   (branch (label null))
   (save continue)
   (save x)
   (assign continue (label after-x))
   (assign x (op cdr) (reg x))
   (goto (label append-loop))
 after-x
   (restore x)
   (assign x (op car) (reg x))
   (assign y (op cons) (reg x) (reg y))
   (restore continue)
   (goto (reg continue))
 null
   (goto (reg continue))
 append-done
   (perform (op print) (reg y)))))
|#