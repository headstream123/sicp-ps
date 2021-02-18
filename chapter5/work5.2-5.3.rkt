#lang sicp

;; work5.3
;; b)
(controller
 sqrt-loop
   (assign x (op read))
   (assign g (const 1.0))
 test-enough
   (assign t (op *) (reg g) (reg g))
   (assigh t (op -) (reg t) (reg x))
   (test (op >) (reg t) (const 0))
   (branch (label sqrt-iter))
   (assign t (op -) (const 0) (reg t))
 sqrt-iter
   (test (op <) (reg t) (const 0.001))
   (branch (label sqrt-done))
   (goto (label improve))
 improve
   (assign t (op /) (reg x) (reg g))
   (assign t (op +) (reg t) (reg g))
   (assign g (op /) (reg t) (const 2))
   (goto (test-enough))
 sqrt-done
   (perform (op print) (reg g))
   (goto (label sqrt-loop)))

;; a)
(controller
 sqrt-loop
   (assign x (op read))
   (assign g (const 1.0))
 test-enough
   (test (op good-enough?) (reg g))
   (branch (label sqrt-done))
   (assign g (op improve) (reg g) (reg x))
   (goto (label test-enough))
 sqrt-done
   (perform (op print) (reg g))
   (goto (label sqrt-loop)))

;; work5.2
(controller
   (assign product (const 1))
   (assign counter (const 1))
   
 test-counter
   (test (op >) (reg counter) (const n))
   (branch (label fact-done))
   (assign product (op *) (reg product) (reg counter))
   (assign counter (op +) (reg counter) (const 1))
   (goto (label test-counter))
 fact-done)