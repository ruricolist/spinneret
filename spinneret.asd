;;;; spinneret.asd

(asdf:defsystem #:spinneret
  :description "Common Lisp HTML5 generator."
  :version "2.0"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:spinneret-tests)))
  :serial t
  :depends-on (#:cl-markdown
               #:parenscript
               #:alexandria
               #:cl-ppcre)
  :components ((:file "package")
               (:file "syntax")
               (:file "tags")
               (:file "spinneret")
               (:file "run")
               (:file "functions")
               (:file "compile")
               (:file "deftag")
               (:file "ps")))

(asdf:defsystem #:spinneret-tests
  :description "Test suite for Spinneret"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on (#:spinneret #:fiveam)
  :perform (asdf:test-op (o c) (uiop:symbol-call :spinneret.tests :run-tests))
  :components ((:file "tests")))
