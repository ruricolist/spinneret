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
       (defun ,fn-name (attrs body pre? empty?)
         (declare (optimize
                   (speed 3) (safety 0) (debug 0)
                   (compilation-speed 0))
                  (function body))
         (let ((html *html*)
               (pretty *print-pretty*)
               (*pre* pre?)
               (*depth* (1+ *depth*))
               (*html-path* (cons ,(make-keyword tag) *html-path*)))
           (declare (dynamic-extent *html-path*))
           ,(if inline?
                '(unless (in-block?)
                  (fresh-line html))
                '(fresh-line html))
           (indent html *depth*)
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
           ,@(unsplice
              (when newline-after-start
                '(elastic-newline html)))
           (unless empty?
             (,@(eif paragraph?
                     '(let ((*indent* (1+ *depth*))))
                     '(let ((*indent* nil))))
              ;; Print the body.
              (without-trailing-space
                (funcall body))))
           ,@(unsplice
              (when newline-before-close
                '(newline-and-indent html)))
           ;; Print the closing tag.
           ,@(unsplice
              (when needs-close?
                `(write-string ,close html)))
           ,@(unsplice
              (when newline-after-close
                `(elastic-newline html)))
           (values))))))

(defmacro define-all-tags ()
  `(progn
     ,@(loop for tag in *html5-elements*
             collect `(define-tag ,tag))))

(define-all-tags)
