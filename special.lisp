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
  (or (bound-value '*indent*)
      *depth*))

(defvar *pre* nil)

(defparameter *fill-column* 80
  "Column at which to wrap text.
This is always measured from the start of the tag.")

(declaim (boolean *pending-space* *suppress-inserted-spaces*))

(defvar *pending-space* nil)

(defvar *suppress-inserted-spaces* nil
  "When set to non-nil, spaces will never be inserted automatically.")

(defvar *html-path* nil
  "List (in ascending order) of parent nodes.")
(assert (null *html-path*))

(defvar *html-style* :human
  "How should we pretty-print HTML?")
(declaim (type (member :human :tree) *html-style*))

(defvar *always-quote* nil
  "Add quotes to all attributes, regardless of whether their value would
otherwise be left unquoted per the HTML5 spec. Useful when generating a
template whose attribute values are placeholders for another system to
substitute into later — the default unquoted output is only ever checked
against the literal placeholder text, not whatever ends up there at
runtime.

Must be set with SETF before the code using it is compiled, not bound
with LET around the call site: whether to quote a given attribute is
decided at macroexpansion time, so a dynamic binding in effect only when
the already-compiled code runs has no effect.")
(declaim (type boolean *always-quote*))
