#lang sicp

;; work4.63
;  rule也可以认为是数据库里的虚拟数据,或者说是数据的抽象集合
;  编写和database中car相同的rule是没有问题的,当然这也和底层实现相关
(rule (grandson ?gf ?gs)
      (and (son ?gf ?f)
           (son ?f ?gs)))

(rule (son ?m ?s)
      (and (wife ?m ?w)
           (son ?w ?s)))

(grandson Cain ?gs)
(son Lamech ?s)
(grandson Methushael ?gs)

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

;; work4.62
(rule (last-pair (?x) (?x)))

(rule (last-pair (?x . ?y) (?z))
      (last-pair ?y (?z)))