;;;; package.lisp

(defpackage #:spinneret
  (:use #:cl #:parenscript #:alexandria
    #:trivial-gray-streams)
  (:export #:with-html #:with-html-string #:html
           #:*html*
           #:*html-lang* #:*html-charset*
           #:get-html-path
           #:do-elements
           #:deftag
           #:*unvalidated-attribute-prefixes*
           #:*boolean-attributes*
           #:*fill-column*
           #:html-length
           #:dynamic-tag
           #:*html-style*
           #:spinneret-error
           #:no-such-tag
           #:*suppress-inserted-spaces*)
  (:shadowing-import-from :alexandria :switch)
  (:import-from :serapeum
    :fmt :eif :econd
    :define-do-macro :defconst
    :nlet :nix :assure
    :find-keyword
    :-> :with-thunk
    :and-let* :op :string-prefix-p
    :memq)
  (:import-from :global-vars :define-global-parameter))

(defpackage #:spinneret-user
  (:use #:cl #:parenscript #:spinneret))
