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
           #:*fill-column*
           #:html-length)
  (:shadowing-import-from :alexandria :switch)
  (:import-from :serapeum
    :fmt :unsplice :string+ :eif
    :define-do-macro
    :nlet :nix :assure)
  (:import-from :global-vars :define-global-parameter))

(defpackage #:spinneret-user
  (:use #:cl #:parenscript #:spinneret))
