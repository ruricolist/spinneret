(in-package #:asdf-user)

(asdf:defsystem #:spinneret.cl-markdown
  :description "Integration with cl-markdown"
  :version "2.0"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-markdown
               #:spinneret)
  :components ((:file "cl-markdown")))
