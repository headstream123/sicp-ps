#lang sicp

;; work4.60
;  很多这种类似问题都需要进行排序,把问题从 = 或 != 变为 < 或 >
;  因为eq符合交换律,所以就会出现重复,而 < 或 > 只能是单边的
(define (person->string person)
  (let ((string (map symbol->string person)))
    (if (null? person)
      ""
      (apply string-append string)))) 

(define (person>? p1 p2)
  (string>? (person->string p1) (person->string p2))) 
  
(rule (neighbor ?p1 ?p2)
      (and (address ?p1 (?town . ?r1))
           (address ?p2 (?town . ?r2))
           (lisp-value person>? ?p1 ?p2)))

;; work4.58
(rule (big-shot ?person)
      (and (job ?person (?division . ?r1))
           (or (not (supervisor ?person ?boss))
               (and (supervisor ?person ?boss)
                    (not (job ?boss (?division . ?r2)))))))

;; work4.57 
;; a)
(replace ?person (Fect Cy D))

;; b)
(and (salary ?person-1 ?amount-1)
     (salary ?person-2 ?amount-2)
     (lisp-value < ?amount-1 ?amount-2)
     (replace ?person-1 ?person-2))

(rule (replace ?person-1 ?person-2)
      (and (job ?person-1 ?work-1)
           (job ?person-2 ?work-2)
           (or (same ?work-1 ?work-2)
               (can-do-job ?work-1 ?work-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))