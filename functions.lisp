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
          (find-symbol tag *tags-pkg*))))

  (-> tag-open (string-designator) (simple-array character (*)))
  (defun tag-open (tag)
    (coerce (fmt "<~(~A~)" tag)
            '(simple-array character (*))))

  (-> tag-close (string-designator) (simple-array character (*)))
  (defun tag-close (tag)
    (if (void? tag)
        ""
        (coerce (fmt "</~(~A~)>" tag)
                '(simple-array character (*))))))

(defmacro define-tag (tag)
  (let* ((inline? (inline? tag))
         (paragraph? (paragraph? tag))
         (fn-name (tag-fn tag :intern t))
         (open (tag-open tag))
         (close (tag-close tag))
         (needs-close? (needs-close? tag)))
    `(progn
       (declaim (notinline ,fn-name))
       (declaim (ftype (function (list function t t) (values))
                       ,fn-name))
       (defun ,fn-name (attrs body pre? empty?)
         (let* ((html *html*)
                (pretty *print-pretty*)
                (style *html-style*)
                (*pre* pre?)
                (*depth* (1+ *depth*))
                (*html-path* (cons ,(make-keyword tag) *html-path*)))
           (declare (dynamic-extent *html-path*))
           ,(econd
              (inline?
               `(print-inline-tag html pretty style
                                  ,open ,(length open)
                                  attrs
                                  empty? body
                                  ,close
                                  ,needs-close?))
              (paragraph?
               `(print-par-tag html pretty style
                               ,open attrs
                               empty? body
                               ,close
                               ,needs-close?))
              (t
               `(print-block-tag html pretty style
                                 ,open attrs
                                 empty? body
                                 ,close
                                 ,needs-close?)))
           (values))))))

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
      (if pretty
          (format-attributes-pretty/block attrs html)
          (format-attributes-plain attrs html)))
    (write-char #\> html))

  (defun open-par (html pretty open attrs)
    (open-block html pretty open attrs))

  (defun open-inline (html pretty open offset attrs)
    (when pretty
      (maybe-wrap offset html))
    (write-string open html)
    (when attrs
      (if pretty
          (format-attributes-pretty/inline attrs html)
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

  (defun close-inline (html close needs-close?)
    (when needs-close?
      (write-string close html)))

  (defun close-block (html close needs-close?)
    (when needs-close?
      (write-string close html)))

  (defun close-par (html close needs-close?)
    (when needs-close?
      (write-string close html))
    (elastic-newline html))

  (defun print-inline-tag (html pretty style open offset attrs empty? body close needs-close?)
    (when (eql style :tree)
      (return-from print-inline-tag
        (print-block-tag html pretty style open attrs empty? body close t)))

    (open-inline html pretty open offset attrs)
    (unless empty?
      (inline-body body))
    (close-inline html close needs-close?))

  (defun print-par-tag (html pretty style open attrs empty? body close needs-close?)
    (when (eql style :tree)
      (return-from print-par-tag
        (print-block-tag html pretty style open attrs empty? body close t)))

    (open-par html pretty open attrs)
    (unless empty?
      (par-body body))
    (close-par html close needs-close?))

  (defun print-block-tag (html pretty style open attrs empty? body close needs-close?)
    (when (eql style :tree)
      (setq needs-close? t))
    (open-block html pretty open attrs)
    (unless empty?
      (block-body html body pretty))
    (close-block html close needs-close?))

  (progn
    (defun dynamic-tag* (tag attrs body &optional empty?)
      "Dynamically select a tag at runtime.
Note that TAG must be a known tag."
      (let* ((tag (or (and-let* ((kw (find-keyword (string-upcase tag)))
                                 (tag (valid? kw))))
                      (error 'no-such-tag :name tag)))
             (open (tag-open tag))
             ;; Note that dynamic tags always print the closing tag --
             ;; not worth the effort to check.
             (close (tag-close tag))
             (*pre* (and (preformatted? tag) t))
             (*depth* (1+ *depth*))
             (*html-path* (cons tag *html-path*))
             (pretty *print-pretty*)
             (style *html-style*))
        (declare (dynamic-extent *html-path*))
        (cond ((inline? tag)
               (print-inline-tag
                *html*
                pretty style
                open (length open) attrs
                empty?
                body
                close t))
              ((paragraph? tag)
               (print-par-tag
                *html*
                pretty style
                open attrs
                empty? body
                close t))
              (t
               (print-block-tag
                *html*
                pretty style
                open
                attrs
                empty?
                (assure function body)
                close t)))
        (values)))

    (define-all-tags)))
