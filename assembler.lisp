;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-microvm)

(defun assemble% (instructions)
  (macrolet ((assemble-operand (instructions)
               `(let ((arg (car ,instructions)))
                  (when (keywordp arg)
                    ;; record the address of the reference to a label
                    (push (+ (length bytes) (length instr))
                          (gethash arg references))
                    ;; reserve space for the actual address
                    (setf arg #xffff))
                  (setf ,instructions (cdr ,instructions))
                  (list (shr arg 8 8) (logand #xff arg)))))
    (loop
       ;; keep track of the labels
       :with label-table := (make-hash-table)
       :with references := (make-hash-table)

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

       :finally (return (resolve-labels bytes references label-table)))))

(defun resolve-labels (bytes references label-table)
  (loop :for label :being :the :hash-keys :in label-table :using (:hash-value addr)
     :do (loop :for reference :in (gethash label references) :do
            (setf (nth reference bytes) (shr addr 8 8)
                  (nth (1+ reference) bytes) (logand #xff addr)))
     :finally (return bytes)))

(defmacro asm (&rest args)
  `(assemble% ',args))

(defun dump (bytes)
  (format t "~{~2,'0x~^ ~}" bytes))
