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

(define-opcode #x02 push (arg))

(define-opcode #x03 pop (arg))

(define-opcode #x04 call (addr)
  (push (pc vm) (stack vm))
  (setf (pc vm) addr))

(define-opcode #x05 inc (arg))

(define-opcode #x06 dec (arg))

(define-opcode #x07 not (arg))

(define-opcode #x08 jmp (address))

(define-opcode #x09 je (address))

(define-opcode #x0a jne (address))

(define-opcode #x0b jg (address))

(define-opcode #x0c jge (address))

(define-opcode #x0d jl (address))

(define-opcode #x0e jle (address))

(define-opcode #x0f prn (integer)
  (print integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2OP
(define-opcode #x10 mov (arg0 arg1))

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

