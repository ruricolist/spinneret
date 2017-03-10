(in-package #:spinneret)

(defpackage #:spinneret.tag
  ;; This package should not import any symbols.
  (:use))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *tags-pkg* (find-package :spinneret.tag))

  (defun unsplice (x)
    (if x (list x) x))

  (defun tag-fn (tag &key intern)
    (let ((tag (string tag)))
      (if intern
          (intern tag *tags-pkg*)
          (find-symbol tag *tags-pkg*)))))

(defmacro define-tag (tag)
  (let* ((fn-name
           (tag-fn tag :intern t))
         (inline? (inline? tag))
         (newline-before-start
           (not inline?))
         (newline-after-start
           (not (or inline? (paragraph? tag))))
         (newline-before-close
           newline-after-start)
         (open (format nil "~(~A~)" tag))
         (needs-close (not (or (void? tag) (unmatched? tag))))
         (close
           (when needs-close
             (format nil "</~(~A~)>" tag))))
    `(progn
       (declaim (notinline ,fn-name))
       (defun ,fn-name (attrs body pre? empty?)
         (declare (optimize
                   (speed 3) (safety 0) (debug 0)
                   (compilation-speed 0))
                  (function body))
         ,@(unsplice
            (when newline-before-start
              `(pprint-newline :mandatory *html*)))
         (,@(if inline? '(progn) `(pprint-logical-block (*html* nil)))
          (let ((pretty *print-pretty*)
                (*pre* pre?)
                (*depth* (1+ *depth*))
                (*html-path* (cons ,(make-keyword tag) *html-path*)))
            (declare (ignorable pretty)
                     (dynamic-extent *html-path*))
            (pprint-logical-block (*html* nil :prefix "<" :suffix ">")
              (write-string ,open *html*)
              (when attrs
                (,(if inline? 'format-attributes/inline 'format-attributes)
                 attrs *html*)))
            (unless empty?
              (pprint-indent :block 1 *html*)
              ,@(unsplice
                 (when newline-after-start
                   `(pprint-newline :mandatory *html*)))
              (without-trailing-space
                ,(if inline?
                     `(funcall body)
                     `(pprint-logical-block (*html* nil)
                        (funcall body))))
              (pprint-indent :block 0)
              ,(if newline-before-close
                   `(pprint-newline :mandatory *html*)
                   `(pprint-newline :linear *html*))
              ,(when close
                 `(write-string ,close *html*)))
            (values)))))))

(defmacro define-all-tags ()
  `(progn
     ,@(loop for tag in *html5-elements*
             collect `(define-tag ,tag))))

(define-all-tags)
