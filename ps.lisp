(in-package #:spinneret)

(defparameter *props*
  '("acceptCharset" "accessKey" "allowTransparency" "bgColor" "cellPadding"
    "cellSpacing" "className" "className" "colSpan" "style" "defaultChecked"
    "defaultSelected" "defaultValue" "htmlFor" "frameBorder" "hSpace" "htmlFor"
    "longDesc" "maxLength" "marginWidth" "marginHeight" "noResize" "noShade"
    "readOnly" "rowSpan" "tabIndex" "vAlign" "vSpace"))

(defparameter *ie-attr-props*
  '(("for"   . "htmlfor")
    ("class" . "classname")))

(ps:define-ps-symbol-macro *html* (ps:@ window spinneret))

(ps:define-ps-symbol-macro *html-charset* (lisp *html-charset*))

(ps:define-ps-symbol-macro *html-lang* (lisp *html-lang*))

(ps:defpsmacro ch (&rest args)
  `(ps:chain ,@args))

(ps:defpsmacro with-html (&rest html-forms)
  (ps:with-ps-gensyms (node d)
    `(let ((,node (or *html*
                      (setf *html* (ch document (create-document-fragment)))))
           (,d document))
       (symbol-macrolet ((*html* ,node)
                         (document ,d))
         ,@(with-standard-io-syntax
             (parse-html html-forms nil)))
       (unless (ps:@ ,node parent-node)
         (prog1 ,node
           (setf *html* nil))))))

(ps:defpsmacro with-tag ((name &rest attributes) &body body)
  `(progn
     (setf *html*
           (ch *html*
               (append-child
                (ch document (create-element ,(string-downcase name))))))
     ,@(loop for (attr val . nil) on attributes by #'cddr
             collect (make-attr-setter (string-downcase attr) val))
     ,@(when body
         (loop for form in body
               if (and (consp form) (eql (car form) 'with-tag))
                 collect form
               else collect `(ch *html* (append-child
                                         (ch document
                                             (create-text-node
                                              (ps:stringify ,form)))))))
     (setf *html* (ps:@ *html* parent-node))
     nil))

(defun make-attr-setter (attr val)
  ;; Compatibility hacks from Laconic.js 0.2.2.
  (let ((attr (or (find
                   (or (cdr (assoc attr *ie-attr-props* :test #'string-equal))
                       attr)
                   *props* :test #'string-equal)
                  attr))
        (sval `(ps:stringify ,val)))
    (flet ((set-or-remove (object attr val)
             (ps:with-ps-gensyms (actual-val)
               `(let ((,actual-val ,val))
                  (if ,actual-val
                      (ch ,object (set-attribute ,attr (ps:stringify ,actual-val)))
                      (ch ,object (remove-attribute ,attr)))))))
      (cond
        ((event? attr)
         ;; Set events as properties, ensuring a href.
         `(setf (ps:@ *html* ,attr) ,sval
                (ps:@ *html* href)
                (or (ps:@ *html* href) "#")))
        ;; Style requires special handling for IE.
        ((string-equal attr "style")
         `(if (ps:@ *html* style set-attribute)
              (ch *html* style (set-attribute 'css-text ,sval))
              (ch *html* (set-attribute ,attr ,sval))))
        ((rassoc attr *ie-attr-props* :test #'string-equal)
         ;; Other special cases for IE.
         `(setf (ps:@ *html* ,attr) ,sval))
        ((data-attr? attr)
         `(setf (ps:@ *html* dataset ,(data-attr-prop attr)) ,sval))
        ((string-equal attr "attrs")
         (ps:with-ps-gensyms (attrs attr)
           `(let ((,attrs ,val))
              (ps:for-in (,attr ,attrs)
                         ,(set-or-remove '*html* attr `(ps:@ ,attrs ,attr))))))
        (t (set-or-remove '*html* attr val))))))

(defun event? (attr)
  (starts-with-subseq "on" (string attr)))

(defun data-attr? (attr)
  (starts-with-subseq "data-" (string attr) :test #'char-equal))

(defun data-attr-prop (attr)
  (substitute #\_ #\-
              (subseq (string-downcase attr)
                      #.(length "data-"))))

(ps:defpsmacro comment (text safe?)
  (declare (ignore safe?))
  `(ps:stringify
    ,(ps::concat-constant-strings
      (list "<!-- " text " -->"))))

(ps:defpsmacro cdata (text safe?)
  (declare (ignore safe?))
  `(ps:stringify
    ,(ps::concat-constant-strings
      (list cdata-start text cdata-end))))

(ps:defpsmacro format-text (formatter &rest args)
  (let ((control-string
          (if (listp formatter)
              (second formatter)
              formatter)))
    (prog1 control-string
      (when args
        (cerror
         "Discard arguments and print \"~A\" literally."
         "Parenscript doesn't have FORMAT."
         control-string)))))

(ps:defpsmacro join-tokens (&rest classes)
  `(ps:stringify
    ,@(ps::concat-constant-strings
       (intersperse " "
                    (remove-duplicates (remove nil classes)
                                       :test #'equal)))))

(defun intersperse (new-elt list)
  (cons (car list)
        (mapcan
         (lambda (elt)
           (list new-elt elt))
         (cdr list))))
