;; titan.scm

(module titan (register titan-assemble)
  (import scheme chicken)

  (define registers '(
		      (R0 . #b00000000) (R1 . #b00000001) (R2 . #b00000010)
		      (R3 . #b00000011) (R4 . #b00000100) (R5 . #b00000101)
		      (R6 . #b00000110) (R7 . #b00000111) (R8 . #b00001000)
		      (R9 . #b00001001) (RA . #b00001010) (RB . #b00001011)
		      (RC . #b00001100) (RD . #b00001101) (RE . #b00001110)
		      (RF . #b00001111)))

  (define (register r)
    (let ((reg (assoc r registers)))
      (if (not reg)
  	(error (string "Invalid register" reg))
        (cdr reg))))

  (define opcodes `(
		    (nop . #\x00) (add . #\x10) (sub . #\x20) (and . #\x30) (or . #\x40) (not . #\x50) (xor . #\x60)
		    (psh . ,(lambda (reg)
		  	      (+ #b01110000 (car reg))))
		    (pop . ,(lambda (reg)
			      (+ #b1000000 (car reg))))
		    (jmp . #\x90) (jpi . #\xA0) (jpz . #\xB0) (jpc . #\xC0) (jps . #\xD0)
		    (stm . ,(lambda (reg)
			      (+ #b11100000 (car reg))))
		    (ldm . ,(lambda (reg)
			      (+ #b11110000 (car reg))))))

  (define (lookup a)
    (let ((result (assoc a opcodes)))
      (if (not result)
          (error (string "Unknown instruction" result))
        (cdr result))))

  (define (titan-assemble file instrs)
    (let ((out (open-output-file file)))
      (for-each (lambda (instr)
		  (let ((opcode (lookup (car instr))))
		    (cond
		      ((not opcode)
		        (error (string "Invalid instruction: " opcode)))
		      ((char? opcode)
		        (write-char opcode out))
		      (else
		        (let ((result (opcode (cdr instr))))
			  (write-char (integer->char result) out))))))
	        instrs)
      (close-output-port out))))

