;;;; package.lisp

(defpackage #:spinneret
  (:use #:cl #:parenscript #:alexandria)
  (:export #:with-html #:with-html-string #:html
           #:*html* #:*html-fill-column* #:*html-min-room*
           #:*html-lang* #:*html-charset*
           #:*html-path*
           #:deftemplate #:do-elements
           #:deftag)
  (:shadowing-import-from :alexandria :switch))
