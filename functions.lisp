(in-package #:spinneret)

(defpackage #:spinneret.tag
  ;; This package should not import any symbols.
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *tags-pkg* (find-package :spinneret.tag))

  (defun tag-fn (tag &key intern)
    (let ((tag (string tag)))
      (if intern
          (intern tag *tags-pkg*)
          (find-symbol tag *tags-pkg*)))))

(defmacro define-tag (tag)
  (let* ((inline? (inline? tag))
         (paragraph? (paragraph? tag))
         (needs-close? (not (or (void? tag) (unmatched? tag))))

         (fn-name (tag-fn tag :intern t))
         (open (fmt "<~(~A~)" tag))
         (close (eif needs-close? (fmt "</~(~A~)>" tag) "")))
    (receive (open-form body-form close-form)
        (econd
         (inline?
          (values
           `(open-inline html pretty ,open ,(length open) attrs)
           '(inline-body body)
           `(close-inline html ,close)))
         (paragraph?
          (values
           `(open-par html pretty ,open attrs)
           '(par-body body)
           `(close-par html ,close)))
         (t
          (values
           `(open-block html pretty ,open attrs)
           '(block-body html body pretty)
           `(close-block html ,close))))
      `(progn
         (declaim (notinline ,fn-name))
         (declaim (ftype (function (list function t t) (values))
                         ,fn-name))
         (defun ,fn-name (attrs body pre? empty?)
           (let ((html *html*)
                 (pretty *print-pretty*)
                 (*pre* pre?)
                 (*depth* (1+ *depth*))
                 (*html-path* (cons ,(make-keyword tag) *html-path*)))
             (declare (dynamic-extent *html-path*))
             ,open-form
             (unless empty?
               ,body-form)
             ,close-form
             (values)))))))

(defmacro define-all-tags ()
  `(progn
     ,@(loop for tag in *html5-elements*
             collect `(define-tag ,tag))))

;;; The auxiliary functions are block-compiled for speed.

(serapeum:local*
  (declaim (optimize (speed 3) (safety 0) (debug 0)
                     (compilation-speed 0)))

  (declaim (inline close-inline close-block))

  (defun open-block (html pretty open attrs)
    (when pretty
      (fresh-line html))
    (write-string open html)
    (when attrs
      (eif pretty (format-attributes-pretty/block attrs html)
           (format-attributes-plain attrs html)))
    (write-char #\> html))

  (defun open-par (html pretty open attrs)
    (open-block html pretty open attrs))

  (defun open-inline (html pretty open offset attrs)
    (when pretty
      (maybe-wrap offset html))
    (write-string open html)
    (when attrs
      (eif pretty (format-attributes-pretty/inline attrs html)
           (format-attributes-plain attrs html)))
    (write-char #\> html))

  (defun block-body (html body pretty)
    (declare (type function body))
    (when pretty
      (elastic-newline html))
    (let ((*indent* (1+ *depth*)))
      (without-trailing-space
        (funcall body)))
    (when pretty
      (terpri html)))

  (defun inline-body (body)
    (declare (type function body))
    (let ((*indent* (1+ *depth*)))
      (without-trailing-space
        (funcall body)))
    (values))

  (defun par-body (body)
    (inline-body body))

  (defun close-inline (html close)
    (write-string close html))

  (defun close-block (html close)
    (write-string close html))

  (defun close-par (html close)
    (write-string close html)
    (elastic-newline html))

  (define-all-tags))
