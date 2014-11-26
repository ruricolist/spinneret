;;;; spinneret.asd

(asdf:defsystem #:spinneret
  :description "Common Lisp HTML5 generator."
  :version "2.0"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-markdown
               #:parenscript
               #:alexandria
               #:optima
               #:cl-ppcre)
  :components ((:file "package")
               (:file "memo")
               (:file "syntax")
               (:file "tags")
               (:file "spinneret")
               (:file "run")
               (:file "functions")
               (:file "compile")
               (:file "deftag")
               (:file "ps")))
