;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-microvm)

;; the machine will have 64Kb of memory
;; 16 bit word size processor
;; 16 16-bit general purpose registers A-P
;; register R for result
;; unlimited? size stack

;; lowest 256? bytes (to be decided) cannot be addressed, those
;; addresses are mapped internally to registers, ports, and other
;; things

;; instructions are encoded as 1 types+opcode byte, and 0/1/2 opearand
;; bytes/words

(defclass microvm ()
  ((memory    :accessor memory    :initform (make-array '(#xffff)))
   (stack     :accessor stack     :initform (list))
   (registers :accessor registers :initform (make-array '(16)))
   (pc        :accessor pc        :initarg  :pc                    :initform 0))
  (:documentation "A virtual machine."))

;; TODO
;; we'll do some magic and forbid access to the lowest 256 bytes of memory
;; we can use that for program metadata, such as the start position
;; we'll also make it so 0-15 refer to the 16 registers, 16 to the result register

(defmethod fetch-byte ((vm microvm) address)
  (elt (memory vm) address))

(defmethod fetch-word ((vm microvm) address)
  (+ (shl (elt (memory vm) address) 16 8)
     (elt (memory vm) (1+ address))))

(defmethod cycle ((vm microvm))
  (let* ((byte (prog1 (elt (memory vm) (pc vm)) (incf (pc vm))))
         (opcode (logand #b00011111 byte))
         (fetch-arg (if (logbitp 7 byte) #'fetch-word #'fetch-byte)))
    (cond ((zerop (logand #b01111110 opcode)) ; 0OP
           (funcall (elt *opcodes* opcode)))
          ((zerop (logand #b00010000 opcode)) ; 1OP
           (funcall (elt *opcodes* opcode)
                    (ecase (shr opcode 1 6)
                      (#b0 (prog1 (funcall fetch-arg vm (pc vm)) (incf (pc vm))))
                      (#b1 (prog1 (funcall fetch-arg vm (pc vm)) (incf (pc vm) 2))))))
          (t                            ; 2OP
           (funcall (elt *opcodes* opcode)
                    (ecase (shr opcode 1 6)
                      (#b0 (prog1 (funcall fetch-arg vm (pc vm)) (incf (pc vm))))
                      (#b1 (prog1 (funcall fetch-arg vm (pc vm)) (incf (pc vm) 2))))
                    (ecase (shr opcode 1 5)
                      (#b0 (prog1 (funcall fetch-arg vm (pc vm)) (incf (pc vm))))
                      (#b1 (prog1 (funcall fetch-arg vm (pc vm)) (incf (pc vm) 2)))))))))
