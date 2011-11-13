(use titan)

(define (test)
  (titan-assemble "a.out"
    `(
      (psh ,(register 'R1))
      (nop)
      (add)
      (stm ,(register 'R4))
      (stm ,(register 'RA))
      (sub)
      (ldm ,(register 'R2))
      (nop))))

