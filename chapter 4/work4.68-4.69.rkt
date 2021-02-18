#lang sicp

;; work4.69
(rule ((great . ?rel) ?x ?y)
      (and (son ?x ?gf)
           (?rel ?gf ?y)
           (last-pair ?rel (grandson))))

(rule ((grandson) ?g ?s) (grandson ?g ?s))

;; Auxiliary
(rule (last-pair (?x) (?x)))

(rule (last-pair (?x . ?y) (?z))
      (last-pair ?y (?z)))

(rule (grandson ?gf ?gs)
      (and (son ?gf ?f)
           (son ?f ?gs)))

(rule (son ?m ?s)
      (and (wife ?m ?w)
           (son ?w ?s)))

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

;; work4.68
;  下面两版reverse的问题在于append-to-form在某一个方向上,总会出现两个变量
;  一个常量的情况,so可能性是无限的,出现无穷流
;  解决方法就是不用append,maybe需要使用lisp-value
(rule (reverse (?x) (?x)))

; (reverse (1 2 3) ?x)可以正确运行,(reverse ?x (1 2 3))不行
; 此版本从内向外验证
(rule (reverse ?x (?u . ?v))
      (and (append-to-form ?w (?t) ?x)
           (append-to-form ?w (?u) ?x)
           (reverse ?w ?v)))

; (reverse ?x (1 2 3))可以正确运行,(reverse (1 2 3) ?x)不行
; 此版本从外向内验证
(rule (reverse ?x ?y)
      (and (append-to-form (?f) ?r ?x)
           (append-to-form ?rv (?f) ?y)
           (reverse ?r ?rv))) 

(rule (append-to-form () ?y ?y))
(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))