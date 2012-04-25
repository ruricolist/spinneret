;;;; package.lisp

(defpackage #:spinneret
  (:use #:cl #:parenscript)
  (:export #:with-html #:with-html-string
           #:*html* #:*html-fill-column* #:*html-min-room*
           #:*check-tags* #:*html-lang* #:*html-charset*))
