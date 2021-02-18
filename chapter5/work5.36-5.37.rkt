#lang sicp

;; work 5.37
;; 这个版本save&restore了所有seq2需要的regs, 另一做法是sr所有regs
;; 但是那样就可能把seq1修改的reg又restore回去了, 故采用这个版本, 比较略
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (needs-register? seq2 first-reg)
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

;; work5.36
;; delete let, append instead of cons
(define (construct-arglist operand-codes)
  (if (null? operand-codes)
      (make-instruction-sequence '() '(argl)
       '((assign argl (const ()))))
      (let ((code-to-get-last-arg
             (append-instruction-sequences
              (car operand-codes)
              (make-instruction-sequence '(val) '(argl)
               '((assign argl (op list) (reg val)))))))
        (if (null? (cdr operand-codes))
            code-to-get-last-arg
            (preserving '(env)
             code-to-get-last-arg
             (code-to-get-rest-args
              (cdr operand-codes)))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign val (op list) (reg val)) ; add
             (assign argl
              (op append) (reg argl) (reg val))))))) ; changed
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))
