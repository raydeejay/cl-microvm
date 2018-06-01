;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-microvm)

(defmacro assemble-operand (instructions)
  `(let ((arg (car ,instructions)))
     (when (keywordp arg)
       (setf arg (gethash arg *labels*)))
     (setf ,instructions (cdr ,instructions))
     (if (< arg 256)
         (list arg)
         (list (shr arg 8 8) (logand #xff arg)))))

(defun assemble% (instructions)
  (loop :with *labels* := (make-hash-table)
     :for mnemonic := (car instructions)
     :while mnemonic

     :for opcode := (or (position mnemonic *opcode-names*) -1)
     :if (keywordp mnemonic)
     :do (setf instructions (cdr instructions)
               (gethash mnemonic *labels*) (length bytes))
     :else
     :collect (prog1 opcode (setf instructions (cdr instructions))) :into bytes
     :end

     :when (< 1 opcode 32)
     :append (assemble-operand instructions) :into bytes

     :when (< 15 opcode)
     :append (assemble-operand instructions) :into bytes

     :finally (return bytes)))

(defmacro asm (&rest args)
  `(assemble% ',args))

(defun dump (bytes)
  (format t "~{~2,'0x~^ ~}" bytes))
