#lang sicp

;; work5.21
;; b)
(define count-leaves-machine
  (make-machine
   '(continue tree n)
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?)
         (list 'car car) (list 'cdr cdr) (list 'print display))
'(
   (assign continue (label count-leaves-done))
   (assign n (const 0))
 tree-loop
   (test (op null?) (reg tree))
   (branch (label null))
   (test (op pair?) (reg tree))
   (branch (label pair))
   (assign n (op +) (reg n) (const 1))
   (goto (reg continue))
 after-car
   (restore tree)
   (assign tree (op cdr) (reg tree))
   (assign continue (label after-cdr))
   (goto (label tree-loop))
 after-cdr
   (restore continue)
   (goto (reg continue))
 null
   (goto (reg continue))
 pair
   (save continue)
   (save tree)
   (assign continue (label after-car))
   (assign tree (op car) (reg tree))
   (goto (label tree-loop))
 count-leaves-done
   (perform (op print) (reg n)))))

;; a)
(define count-leaves-machine
  (make-machine
   '(continue tree n)
   (list (list '+ +) (list 'null? null?) (list 'pair? pair?)
         (list 'car car) (list 'cdr cdr) (list 'print display))
'(
   (assign continue (label count-leaves-done))
 tree-loop
   (test (op null?) (reg tree))
   (branch (label null))
   (test (op pair?) (reg tree))
   (branch (label pair))
   (assign n (const 1))
   (goto (reg continue))
 pair
   (save continue)
   (assign continue (label aftertree-car))
   (save tree)
   (assign tree (op car) (reg tree))
   (goto (label tree-loop))
 aftertree-car
   (restore tree)
   (assign tree (op cdr) (reg tree))
   (assign continue (label aftertree-cdr))
   (save n)
   (goto (label tree-loop))
 aftertree-cdr
   (restore tree)
   (restore continue)
   (assign n (op +) (reg n) (reg tree))
   (goto (reg continue))
 null
   (assign n (const 0))
   (goto (reg continue))
 count-leaves-done
   (perform (op print) (reg n)))))