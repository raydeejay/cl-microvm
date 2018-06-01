;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-microvm)

(defmacro assemble-operand (instructions)
  `(let ((arg (car ,instructions)))
     (setf ,instructions (cdr ,instructions))
     (if (< arg 256)
         (list arg)
         (list (shr arg 8 8) (logand #xff arg)))))


(defun assemble% (instructions)
  (loop :for mnemonic := (car instructions)
     :while mnemonic

     :for opcode := (position mnemonic *opcode-names*)
     :collect (prog1 opcode (setf instructions (cdr instructions)))

     :when (< 1 opcode 32)
     :append (assemble-operand instructions)

     :when (< 15 opcode)
     :append (assemble-operand instructions)))

(defmacro asm (&rest args)
  `(assemble% ',args))

(defun dump (bytes)
  (format t "~{~2,'0x~^ ~}" bytes))
