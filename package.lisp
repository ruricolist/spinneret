;;;; package.lisp

(defpackage #:spinneret
  (:use #:cl)
  (:export #:with-html #:with-html-string
           #:*html* #:*html-fill-column* #:*html-min-room*
           #:*tag-checking* #:*html-lang* #:*html-charset*))
