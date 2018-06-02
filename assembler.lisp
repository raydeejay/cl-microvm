;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-microvm)

(defmacro assemble-operand (instructions)
  `(let ((arg (car ,instructions)))
     (when (keywordp arg)
       (setf arg (gethash arg *labels*)))
     (setf ,instructions (cdr ,instructions))
     (if (< arg 256)
         (list arg)
         (progn ;; modify opcode if necessary
           (setf instr (cons (logior (car instr) #b10000000) (cdr instr)))
           (list (shr arg 8 8) (logand #xff arg))))))

(defun assemble% (instructions)
  (loop :with *labels* := (make-hash-table)
     :for mnemonic := (car instructions)
     :while mnemonic

     :for opcode := (or (position mnemonic *opcode-names*) -1)
     :for instr := (list)

     :if (keywordp mnemonic)
     :do (setf (gethash mnemonic *labels*) (length bytes))
     :else
     ;; :collect opcode :into bytes
     :do (push opcode instr)
     :end

     :do (setf instructions (cdr instructions))

     :when (< 1 opcode 32)
     ;; :append (assemble-operand instructions) :into bytes
     :do (nconc instr (assemble-operand instructions))

     :when (< 15 opcode)
     ;; :append (assemble-operand instructions) :into bytes
     :do (nconc instr (assemble-operand instructions))

     :append instr :into bytes

     :finally (return bytes)))

(defmacro asm (&rest args)
  `(assemble% ',args))

(defun dump (bytes)
  (format t "~{~2,'0x~^ ~}" bytes))
