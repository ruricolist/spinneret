;;;; package.lisp

(defpackage #:spinneret
  (:use #:cl #:parenscript #:alexandria)
  (:export #:with-html #:with-html-string #:html
           #:*html* #:*html-fill-column* #:*html-min-room*
           #:*html-lang* #:*html-charset*
           #:get-html-path
           #:do-elements
           #:deftag
           #:*unvalidated-attribute-prefixes*)
  (:shadowing-import-from :alexandria :switch)
  (:import-from :global-vars :define-global-parameter))
