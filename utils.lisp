;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:cl-microvm)

(defun shl (x width bits)
  "Compute bitwise left shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x bits)
          (1- (ash 1 width))))

(defun shr (x width bits)
  "Compute bitwise right shift of x by 'bits' bits, represented on 'width' bits"
  (logand (ash x (- bits))
          (1- (ash 1 width))))

(defun rotl (x width bits)
  "Compute bitwise left rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (mod bits width))
                  (1- (ash 1 width)))
          (logand (ash x (- (- width (mod bits width))))
                  (1- (ash 1 width)))))

(defun rotr (x width bits)
  "Compute bitwise right rotation of x by 'bits' bits, represented on 'width' bits"
  (logior (logand (ash x (- (mod bits width)))
                  (1- (ash 1 width)))
          (logand (ash x (- width (mod bits width)))
                  (1- (ash 1 width)))))

(defun signed (n)
  "Numbers are usually stored in 2 bytes (in the form
most-significant-byte first, then least-significant) and hold any
value in the range $0000 to $ffff (0 to 65535 decimal).

These values are sometimes regarded as signed, in the range -32768 to
32767. In effect -n is stored as 65536-n and so the top bit is the
sign bit."
  (if (logbitp 15 n)
      (- (- 65536 n))
      n))
