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

(defmethod fetch-byte ((vm microvm) address &key advance)
  "Get an 8-bit value from the current position of memory, optionally
advancing the program counter by q."
  (prog1 (elt (memory vm) address)
    (when advance (incf (pc vm)))))

(defmethod fetch-word ((vm microvm) address &key advance)
  "Get a 16-bit value from the current position of memory, optionally
advancing the program counter by 2."
  (prog1 (+ (shl (elt (memory vm) address) 16 8)
            (elt (memory vm) (1+ address)))
    (when advance (incf (pc vm)) 2)))

(defmethod load-code ((vm microvm) bytes &optional (address 0))
  "Load code from a list of bytes into the VM memory, at a given
address."
  (loop :for addr :from address
     :for byte :in bytes
     :doing (setf (elt (memory vm) addr) byte)))

(defmethod cycle ((vm microvm))
  "Execute one instruction."
  (let* ((byte (prog1 (elt (memory vm) (pc vm)) (incf (pc vm))))
         (opcode (logand #b00011111 byte))
         (fetch-arg (if (logbitp 7 byte) #'fetch-word #'fetch-byte)))
    (cond ((zerop (logand #b01111110 opcode)) ; 0OP
           (funcall (elt *opcodes* opcode) vm))
          ((zerop (logand #b00010000 opcode)) ; 1OP
           (funcall (elt *opcodes* opcode) vm
                    (funcall fetch-arg vm
                             (case (shr opcode 1 6)
                               (#b0 (pc vm))
                               (#b1 (funcall fetch-arg vm (pc vm))))
                             :advance t)))
          (t                            ; 2OP
           (funcall (elt *opcodes* opcode) vm
                    (funcall fetch-arg vm
                             (case (shr opcode 1 6)
                               (#b0 (pc vm))
                               (#b1 (funcall fetch-arg vm (pc vm))))
                             :advance t)
                    (funcall fetch-arg vm
                             (case (shr opcode 1 6)
                               (#b0 (pc vm))
                               (#b1 (funcall fetch-arg vm (pc vm))))
                             :advance t))))))

(defmethod run ((vm microvm) &optional (address 0))
  "Execute code until the opcode is RET when the stack is empty."
  (loop :initially (setf (pc vm) address
                         (stack vm) (list address))
     :while (stack vm)
     :doing (cycle vm)))
