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

         (fn-name
           (tag-fn tag :intern t))
         (newline-after-start
           (not (or inline? paragraph?)))
         (newline-before-close
           newline-after-start)
         (newline-after-close
           paragraph?)
         (open (fmt "<~(~A~)" tag))
         (close (and needs-close? (fmt "</~(~A~)>" tag))))
    `(progn
       (declaim (notinline ,fn-name))
       (declaim (ftype (function (list function t t) (values))
                       ,fn-name))
       (defun ,fn-name (attrs body pre? empty?)
         (declare (optimize
                   (speed 3) (safety 0) (debug 0)
                   (compilation-speed 0))
                  (type function body)
                  (type list attrs))
         (let ((html *html*)
               (pretty *print-pretty*)
               (*pre* pre?)
               (*depth* (1+ *depth*))
               (*html-path* (cons ,(make-keyword tag) *html-path*)))
           (declare (dynamic-extent *html-path*))
           (when pretty
             ,(if inline?
                  `(maybe-wrap ,(length open) html)
                  '(fresh-line html)))
           ;; Print the opening tag.
           (write-string ,open html)
           (when attrs
             (eif pretty
                  (,(eif inline?
                         'format-attributes-pretty/inline
                         'format-attributes-pretty/block)
                   attrs html)
                  (format-attributes-plain attrs html)))
           (write-char #\> html)
           (unless empty?
             ,(when newline-after-start
                '(when pretty
                  (elastic-newline html)))
             (let ((*indent* (1+ *depth*)))
               ;; Print the body.
               (without-trailing-space
                 (funcall body)))
             ,(when newline-before-close
                '(when pretty
                  (terpri html))))
           ;; Print the closing tag.
           ,@(unsplice
              (when needs-close?
                `(write-string ,close html)))
           ,@(unsplice
              (when newline-after-close
                '(elastic-newline html)))
           (values))))))

(defmacro define-all-tags ()
  `(progn
     ,@(loop for tag in *html5-elements*
             collect `(define-tag ,tag))))

(define-all-tags)
