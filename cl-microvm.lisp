;;;; cl-microvm.lisp

(in-package #:cl-microvm)

(defun main (filename)
  (let ((vm (make-instance 'microvm)))
    (load-program vm filename)
    (run vm)))
