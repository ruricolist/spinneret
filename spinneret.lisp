;;;; spinneret.lisp

(in-package #:spinneret)

(define-condition spinneret-error (error)
  ())

(define-condition no-such-tag (spinneret-error)
  ((name :initarg :name))
  (:report (lambda (c s)
             (with-slots (name) c
               (format s "No such HTML tag: ~a" name)))))

;;;; The exported macros.

(defun get-html-path ()
  "Return a copy of *HTML-PATH*.
This is necessary because *HTML-PATH* itself is stack-allocated."
  (copy-list *html-path*))

(defmacro with-html-path (value &body body)
  "Modify *HTML-PATH* to VALUE in the scope of BODY.
Useful to override the behavior of tags (like :H*) dependent on *HTML-PATH*."
  `(let ((*html-path* ,value))
     ,@body))

(defmacro with-html (&body body &environment env)
  "Interpret BODY as HTML. Consult README.txt for the syntax."
  `(let ((*html* (ensure-html-stream *html*)))
     ,(if (and (null (cdr body)) (atom (car body)))
          (car body)
          `(progn ,@(parse-html body env)))))

(defmacro with-html-string (&body body)
  "Like WITH-HTML, but capture the output as a string."
  `(with-output-to-string (*html*)
     (with-html ,@body)))
