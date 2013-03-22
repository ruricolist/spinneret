;;;; spinneret.lisp

(in-package #:spinneret)

;;;; The exported macros.

(declaim (stream *html*))

(defparameter *html* *standard-output*
  "Output stream for HTML generation.")

(defvar *html-path* nil
  "List (in ascending order) of parent nodes.")

(defmacro with-html (&body body &environment env)
  "Interpret BODY as HTML. Consult README.txt for the syntax."
  (if (and (null (cdr body)) (atom (car body)))
      (car body)
      `(progn ,@(parse-html body env))))

(defmacro with-html-string (&body body)
  "Like WITH-HTML, but capture the output as a string."
  `(with-output-to-string (*html*)
     (with-html ,@body)))
