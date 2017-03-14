;;;; package.lisp

(defpackage #:spinneret
  (:use #:cl #:parenscript #:alexandria)
  (:export #:with-html #:with-html-string #:html
           #:*html*
           #:*html-lang* #:*html-charset*
           #:get-html-path
           #:do-elements
           #:deftag
           #:*unvalidated-attribute-prefixes*)
  (:shadowing-import-from :alexandria :switch)
  (:import-from :serapeum
    :fmt :unsplice :string+ :eif
    :define-do-macro
    :nlet)
  (:import-from :global-vars :define-global-parameter))
