;;;; cl-pci.asd

(asdf:defsystem #:cl-pci
  :serial t
  :description "Programming Collective Intelligence examples in CL."
  :author "Kyle Isom"
  :license "ISC license"
  :depends-on (#:st-json)
  :components ((:file "package")
               (:file "cl-pci")
               (:file "utils")
               (:file "chapter2")))
