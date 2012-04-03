;;;; spinneret.lisp

(in-package #:spinneret)

;;;; The exported macros.

(declaim (stream *html*))

(defparameter *html* *standard-output*
  "Output stream for HTML generation.")

(defmacro with-html (&body body &environment env)
  "Interpret BODY as HTML. Consult README.txt for the syntax."
  `(progn ,@(parse-html body env)
          (maybe-report-invalid-elements)))

(defmacro with-html-string (&body body)
  "Like WITH-HTML, but capture the output as a string."
  `(with-output-to-string (*html*)
     (with-html ,@body)))
