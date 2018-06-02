;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-microvm)

(defun assemble% (instructions)
  (macrolet ((assemble-operand (instructions)
               `(let ((arg (car ,instructions)))
                  (when (keywordp arg)
                    (setf arg (gethash arg label-table)))
                  (setf ,instructions (cdr ,instructions))
                  (if (< arg 256)
                      (list arg)
                      (progn ;; modify opcode if necessary
                        (setf instr (cons (logior (car instr) #b10000000) (cdr instr)))
                        (list (shr arg 8 8) (logand #xff arg)))))))
    (loop :with label-table := (make-hash-table)
       ;; grab the first available symbol
       :for mnemonic := (car instructions)

       ;; go ahead and try to determine the opcode
       :for opcode := (or (position mnemonic *opcode-names*) -1)

       ;; begin with an empty instruction
       :for instr := (list)

       ;; stop when we reach the end of the code
       :while mnemonic

       ;; move to the next symbol
       :do (setf instructions (cdr instructions))

       ;; if we find a keyword, record the position of a label
       :if (keywordp mnemonic)
       :do (setf (gethash mnemonic label-table) (length bytes))
       :else
       ;; if it's an assembler directive, process it
       :if (char-equal #\. (elt (symbol-name mnemonic) 0))
       :do (case mnemonic
             (.DS (let ((str (map 'list #'char-code (car instructions))))
                    (setf instr (cons (length str) str)))
                  (setf instructions (cdr instructions)))
             (.DB (setf instr (list (car instructions)))
                  (setf instructions (cdr instructions)))
             (.DW (let ((num (car instructions)))
                    (setf instr (list (shr num 8 8)
                                      (logand #xff num))))
                  (setf instructions (cdr instructions))))
       :else
       ;; it's an operator and we have an opcode
       :do (push opcode instr)
       :end
       :end

       ;; grab a parameter if this is a 2OP instruction
       :when (< 1 opcode 32)
       :do (nconc instr (assemble-operand instructions))

       ;; grab a parameter if this is a 1OP/2OP instruction
       :when (< 15 opcode)
       :do (nconc instr (assemble-operand instructions))

       ;; add the assembled instruction to the current run of bytes
       :append instr :into bytes

       :finally (return bytes))))

(defmacro asm (&rest args)
  `(assemble% ',args))

(defun dump (bytes)
  (format t "~{~2,'0x~^ ~}" bytes))
