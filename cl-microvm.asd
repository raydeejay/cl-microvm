;;;; cl-microvm.asd

(asdf:defsystem #:cl-microvm
  :description "Describe cl-microvm here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "vm")
               (:file "opcodes")
               (:file "cl-microvm")))
