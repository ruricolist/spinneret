(in-package #:spinneret)

(declaim (stream *html*))

(defparameter *html* (make-synonym-stream '*standard-output*)
  "Output stream for HTML generation.")

(declaim (string *html-lang* *html-charset*))

(defparameter *html-lang* "en")

(defparameter *html-charset* "UTF-8")

(declaim (type (integer -1 #.(1- most-positive-fixnum)) *depth*))

(defvar *depth* -1
  "Depth of the tag being output.")

(defvar *indent*)

(defun get-indent ()
  (or (serapeum:bound-value '*indent*)
      *depth*))

(defvar *pre* nil)

(defparameter *fill-column* 80
  "Column at which to wrap text.
This is always measured from the start of the tag.")

(declaim (boolean *pending-space*))

(defvar *pending-space* nil)

(serapeum:defvar-unbound *html-path*
  "List (in ascending order) of parent nodes.")
