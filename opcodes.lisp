;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-microvm)

;; when the opcode is executed the PC is already past the operands

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OPCODES
(defun illegal (&rest args)
  (declare (ignore args))
  (error "Illegal opcode"))

(defparameter *opcodes* (make-array '(32) :initial-element #'illegal))
(defparameter *opcode-names* (make-array '(32) :initial-element 'illegal))

(defmacro define-opcode (number name args &body body)
  `(let ((fn (lambda (vm ,@args) ,@body)))
     (setf (elt *opcodes* ,number) fn
           (elt *opcode-names* ,number) ',name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 0OP
(define-opcode #x00 nop ())

(define-opcode #x01 ret ()
  (setf (pc vm) (pop (stack vm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1OP

;; some (or all) operators can take both an address (which can
;; represent memory/registers/ports/stack?) or a literal/constant

;; operators can take both an indirection (which can represent
;; memory/registers/ports/stack?) or a literal/constant (which can
;; represent either a numerical value or a memory address

;; w tt nnnnn

;; w represents the width of the operators
;; (0 means byte-sized, 1 means word-sized)

;; types are encoded as the two most significant bytes of the byte
;; holding an opcode:
;;   0 being a constant/literal value,
;;   1 an indirected value

;; there are 32 available operators, 4 are unassigned

(define-opcode #x02 push (addr)
  (push (elt (memory vm) addr) (stack vm)))

(define-opcode #x03 pop (addr)
  (setf (elt (memory vm) addr)
        (pop (stack vm))))

(define-opcode #x04 call (addr)
  (push (pc vm) (stack vm))
  (setf (pc vm) addr))

(define-opcode #x05 inc (arg)
  (incf (elt (memory vm) arg)))

(define-opcode #x06 dec (addr)
  (decf (elt (memory vm) addr)))

(define-opcode #x07 not (arg)
  (setf (elt (memory vm) arg) (lognot (elt (memory vm) arg))))

(define-opcode #x08 jmp (address)
  (setf (pc vm) address))

;; presumably these use either registers (TBI) or stack (implemented)

;; we'll go with stack for now because it's simpler, as using
;; registers requires deciding where they will reside, and
;; implementing MOV

(define-opcode #x09 je (address)
  (when (= (pop (stack vm)) (pop (stack vm)))
    (setf (pc vm) address)))

(define-opcode #x0a jne (address)
  (when (/= (pop (stack vm)) (pop (stack vm)))
    (setf (pc vm) address)))

(define-opcode #x0b jg (address))

(define-opcode #x0c jge (address))

(define-opcode #x0d jl (address))

;; (define-opcode #x0e jle (address))

(define-opcode #x0e prs (address)
  (let ((len (elt (memory vm) address)))
    (print (map 'string #'code-char (subseq (memory vm) (1+ address) (+ 1 address len))))))

(define-opcode #x0f prn (address)
  (print (fetch-word vm address)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2OP
(define-opcode #x10 mov (arg0 arg1)
  ;; hacked together implementation, needs more work
  ;; read value arg1, place at address arg0
  ;; when address is lower than 16, use a register?
  (setf (elt (memory vm) arg0) arg1))

(define-opcode #x11 add (dest src))

(define-opcode #x12 sub (dest src))

(define-opcode #x13 mul (dest src))

(define-opcode #x14 div (dest src))

(define-opcode #x15 mod (arg0 arg1)) ; stores result in the R (result) register

(define-opcode #x16 xor (arg0 arg1))

(define-opcode #x17 or (arg0 arg1))

(define-opcode #x18 and (arg0 arg1))

(define-opcode #x19 shl (arg0 arg1))

(define-opcode #x1a shr (arg0 arg1))

(define-opcode #x1b cmp (arg0 arg1))

