(defsystem "spinneret"
  :description "Common Lisp HTML5 generator."
  :version "3.0"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :in-order-to ((test-op (test-op "spinneret/tests")))
  :serial t
  :depends-on ("alexandria"
               "cl-ppcre"
               "global-vars"
               "serapeum"
               "trivia"
               "trivial-gray-streams"
               "in-nomine")
  :components ((:file "package")
               (:file "special")
               (:file "stream")
               (:file "syntax")
               (:file "tags")
               (:file "spinneret")
               (:file "run")
               (:file "functions")
               (:file "compile")
               (:file "deftag")
               (:file "dynamic")
               (:file "interpret")))

(defsystem "spinneret/cl-markdown"
  :description "Integration with cl-markdown"
  :version "3.0"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :depends-on ("cl-markdown"
               "spinneret")
  :components ((:file "cl-markdown")))

(defsystem "spinneret/ps"
  :description "Integration with Parenscript."
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :serial t
  :depends-on ("spinneret" "parenscript")
  :components ((:file "ps")))

(defsystem "spinneret/tests"
  :description "Test suite for Spinneret"
  :author "Paul M. Rodriguez <pmr@ruricolist.com>"
  :license "MIT"
  :depends-on ("fiveam"
               "spinneret"
               "spinneret/cl-markdown"
               "spinneret/ps")
  :perform (test-op (o c) (symbol-call :spinneret.tests :run-tests))
  :components ((:file "tests")))
