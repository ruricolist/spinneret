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
         (newline-before-start
           (not inline?))
         (newline-after-start
           (not (or inline? paragraph?)))
         (newline-before-close
           newline-after-start)
         (open (fmt "~(~A~)" tag))
         (close (and needs-close? (fmt "</~(~A~)>" tag))))
    `(progn
       (declaim (notinline ,fn-name))
       (defun ,fn-name (attrs body pre? empty?)
         (declare (optimize
                   (speed 3) (safety 0) (debug 0)
                   (compilation-speed 0))
                  (function body))
         (let ((*pre* pre?)
               (*depth* (1+ *depth*))
               (*html-path* (cons ,(make-keyword tag) *html-path*)))
           (declare (dynamic-extent *html-path*))
           ,@(unsplice
              (when newline-before-start
                `(pprint-newline :mandatory *html*)))
           (,@(if inline? '(progn) `(pprint-logical-block (*html* nil)))
            ;; Print the opening tag.
            (pprint-logical-block (*html* nil :prefix "<" :suffix ">")
              (write-string ,open *html*)
              (when attrs
                (,(if inline? 'format-attributes/inline 'format-attributes)
                 attrs *html*)))
            (unless empty?
              ;; Print the body.
              (pprint-indent :block 1 *html*)
              ,@(unsplice
                 (when newline-after-start
                   `(pprint-newline :mandatory *html*)))
              (without-trailing-space
                (funcall body)))
            ;; Print the closing tag.
            ,(if newline-before-close
                 `(progn
                    (pprint-indent :block 0 *html*)
                    (pprint-newline :mandatory *html*))
                 `(pprint-newline :linear *html*))
            ,@(unsplice
               (when needs-close?
                 `(write-string ,close *html*))))
           (values))))))

(defmacro define-all-tags ()
  `(progn
     ,@(loop for tag in *html5-elements*
             collect `(define-tag ,tag))))

(define-all-tags)
