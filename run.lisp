;;;; Functions used at run time or compile time.

(in-package #:spinneret)

(declaim (type (integer -1 #.(1- most-positive-fixnum)) *depth*))

(defvar *depth* -1
  "Depth of the tag being output.")

(defvar *block-start*)

(defun get-block-start ()
  (or (serapeum:bound-value '*block-start*)
      *depth*))

(defun in-block? ()
  (serapeum:bound-value '*block-start*))

(defvar *pre* nil)

(defparameter *fill-column* 80
  "Column at which to wrap text.
This is always measured from the start of the tag.")

(defun fast-format (stream control-string &rest args)
  "Like `format', but bind `*print-pretty*' to nil."
  (declare (dynamic-extent args))
  (let ((*print-pretty* nil))
    (format stream "~?" control-string args)))

(define-compiler-macro fast-format (&whole call stream control-string &rest args)
  (if (stringp control-string)
      (if (equalp control-string "~a")
          (destructuring-bind (arg) args
            `(princ ,arg ,stream))
          `(fast-format ,stream (formatter ,control-string) ,@args))
      call))

(defun indent (&optional (stream *html*) (col *depth*))
  (when *print-pretty*
    (let (*print-pretty*)
      (format stream "~V,0T" col)))
  col)

(defun newline-and-indent (&optional (stream *html*))
  "Fresh line and indent according to *DEPTH*."
  (when *print-pretty*
    (let (*print-pretty*)
      (format stream "~&~V,0T" *depth*))))

(defmacro without-trailing-space (&body body)
  `(let ((*pending-space* nil))
     ,@body))

(defmacro with-space (&body body)
  `(progn
     (flush-space)
     ,@body
     (buffer-space)))

(declaim (boolean *pending-space*))

(defvar *pending-space* nil)

(declaim (inline buffer-space flush-space))

(defun buffer-space ()
  (setf *pending-space* t))

(defun flush-space ()
  (when *pending-space*
    (setf *pending-space* nil)
    (write-char #\Space *html*)))

(defmacro catch-output (arg)
  (typecase arg
    (null nil)
    (string `(fill-text ,(escape-string arg) t))
    ((or character number)              ;not symbol, because not evaluated.
     `(fill-text ,(escape-string (princ-to-string arg)) t))
    (t `(html ,arg))))

(defgeneric html (object)
  (:method (object)
    (declare (ignore object))
    (values))
  (:method ((nada null))
    (values))
  (:method :around ((nada null))
    (values))
  (:documentation "Return an unescaped, unfilled string representing OBJECT."))

(defmethod html :around (object)
  (declare (ignore object))
  (with-space
    (call-next-method))
  (values))

(defmethod html ((string string))
  (if *print-pretty*
      (fill-text (escape-string string) t)
      (escape-to-stream string #'escape-string-char *html*)))

(defmethod html ((char character))
  (if-let (escape (escape-string-char char))
    (write-string escape *html*)
    (write-char char *html*)))

(defmethod html ((n number))
  (fast-format *html* "~d" n))

(defmethod html ((sym symbol))
  (fast-format *html* "~a" sym))

(defun mklist (x) (if (listp x) x (list x)))

(defun call/words (thunk string)
  (let ((window (make-array 0
                            :element-type (array-element-type string)
                            :adjustable t
                            :displaced-to string
                            :displaced-index-offset 0))
        (thunk (ensure-function thunk)))
    (loop with len = (length string)
          for left = 0 then (+ right 1)
          for right = (or (position-if #'whitespace string :start left) len)
          unless (= left right)
            do (adjust-array window (- right left)
                             :displaced-to string
                             :displaced-index-offset left)
               ;; NB In terms of *words*, this might seem wrong: the
               ;; remainder of the string might just be whitespace.
               ;; However, this is the behavior we want: the presence
               ;; of trailing whitespace *should* be preserved.
               (funcall thunk window (= right len))
          until (= right len))))

(define-do-macro do-words ((var at-end? string &optional return) &body body)
  (serapeum:with-thunk (body var at-end?)
    `(call/words ,body ,string)))

(defun fill-text (string &optional safe? &aux (html *html*))
  (check-type string string)
  (cond
    ((= (length string) 0))
    (*pre*
     (fast-format html "~&~A~%" string))
    (*print-pretty*
     (let* ((start-col *depth*)
            (fill *fill-column*)
            (goal (+ fill start-col)))
       (when (whitespace (aref string 0))
         (write-char #\Space html))
       (indent html start-col)
       (flet ((wrap ()
                (terpri html)
                (indent html start-col)))
         (declare (dynamic-extent #'wrap))
         (do-words (word at-end? string)
           (let* ((word (if safe? word (escape-string word)))
                  (len (length word)))
             (cond ((> len fill)
                    (wrap)
                    (write-string word html)
                    (wrap))
                   ((> (+ len (html-stream-column html))
                       goal)
                    (wrap)
                    (write-string word html))
                   (t (write-string word html))))
           (unless at-end?
             (write-char #\Space html))))))
    (t
     (with-space
       (if safe?
           (write-string string *html*)
           (escape-to-stream string #'escape-string-char *html*)))))
  (values))

(defun format-attribute-value (value)
  (cond ((equal value "") "\"\"")
        ((keywordp value) (string-downcase value))
        ((eql value t) "true")
        (t value)))

(defun format-attributes-with (attrs print-boolean print-value)
  "Format ATTRS, uses the unary function PRINT-BOOLEAN to print
Boolean attributes, and the binary function PRINT-VALUE to print
ordinary attributes."
  (serapeum:fbind (print-boolean print-value)
    (let ((seen '()))
      ;; Ensure that the leftmost keyword has priority,
      ;; as in function lambda lists.
      (labels ((seen? (name)
                 (declare (optimize speed)
                          (symbol name))
                 (prog1 (member name seen)
                   (push name seen)))
               (format-attr (attr value)
                 (unless (or (null value) (seen? attr))
                   (if (boolean? attr)
                       (print-boolean attr)
                       (let ((value (format-attribute-value value)))
                         (print-value attr value)))))
               (dynamic-attrs (attrs)
                 (doplist (a v attrs)
                   (format-attr a (escape-value v)))))
        (declare (inline seen?))
        (doplist (attr value attrs)
          (if (eql attr :attrs)
              (dynamic-attrs value)
              (format-attr attr value)))))))

(defun format-attributes-plain (attrs &optional (stream *html*))
  (flet ((format-boolean (attr)
           (format stream " ~(~a~)" attr))
         (format-value (attr value)
           (format stream " ~(~a~)=~a" attr value)))
    (declare (dynamic-extent #'format-boolean #'format-value))
    (format-attributes-with attrs #'format-boolean #'format-value)))

(defun format-attributes-pretty/inline (attrs &optional (stream *html*))
  (declare (stream stream))
  (let* ((start-col (get-block-start))
         (fill *fill-column*)
         (goal (+ start-col fill)))
    (serapeum:fbind* ((too-long?
                       (if *print-pretty*
                           (lambda (len)
                             (> (+ len (html-stream-column stream))
                                goal))
                           (constantly nil)))
                      (print-boolean
                       (lambda (attr)
                         (let ((len (length (symbol-name attr))))
                           ;; No valid attribute is longer than 80. (I
                           ;; suppose a data attribute could be.)
                           (if (too-long? len)
                               (format stream "~%~V,0T~(~a~)" (1- start-col) attr)
                               (progn
                                 (format stream " ~(~a~)" attr))))))
                      (print-attr
                       (lambda (attr value)
                         (let ((len (1+ (length (symbol-name attr)))))
                           (if (too-long? len)
                               (format stream "~%~V,0T~(~a~)=" (1- start-col) attr)
                               (format stream " ~(~a~)=" attr)))
                         (format stream "~a" value))))
      (declare (dynamic-extent #'print-boolean #'print-attr))
      (format-attributes-with attrs
                              #'print-boolean
                              #'print-attr))))

(defun format-attributes-pretty/block (attrs &optional (stream *html*))
  (declare (html-stream stream))
  (let ((*fill-column* (truncate *fill-column* 2))
        (*block-start* (+ (html-stream-column stream)
                          ;; Force the attributes to line up.
                          2)))
    (format-attributes-pretty/inline attrs stream)))

(defun escape-value (value)
  (if (or (eq value t)
          (eq value nil)
          (keywordp value))
      value
      (let ((string (escape-attribute-value
                     (princ-to-string value))))
        (if (needs-quotes? string)
            (concatenate 'string "\"" string "\"")
            string))))

(defun format-text (control-string &rest args)
  (when *print-pretty*
    (terpri *html*))
  (let ((*depth* (1+ *depth*)))
    (fill-text (format nil "~?" control-string args) t))
  (values))

(defun xss-escape (arg)
  "Possibly escape ARG for use with FORMAT.

We don't want to leave ourselves open to XSS, but we also want to be
able to use directives like ~c, ~d, ~{~} &c."
  (typecase arg
    ((or number character symbol)
     arg)
    (list
     (mapcar #'xss-escape arg))
    (t
     (escape-to-string arg))))

(defun make-doctype (&rest args)
  (declare (ignore args))
  `(doctype))

(defun doctype (&rest args)
  (declare (ignore args))
  (format *html* "<!DOCTYPE html>~%"))

(defun make-comment (text)
  `(comment ,(if (stringp text)
                 (escape-comment text)
                 text)
     ,(stringp text)))

(defun comment (text safe?)
  (if *print-pretty*
      (let ((*depth* (+ *depth* 1)))
        (format *html* "~&~v,0T<!-- " *depth*)
        (fill-text (if safe?
                       text
                       (escape-comment text))
                   t)
        (format *html* " -->~%"))
      (progn
        (write-string "<!-- " *html*)
        (write-string
         (if safe?
             text
             (escape-comment text))
         *html*)
        (write-string " -->" *html*))))

(defun make-cdata (text)
  `(cdata ,(if (stringp text)
               (escape-cdata text)
               text)
          ,(stringp text)))

(defun cdata (text safe? &aux (html *html*))
  (write-string cdata-start html)
  (write-string (if safe?
                    text
                    (escape-cdata text))
                html)
  (write-string cdata-end html))

(declaim (string *html-lang* *html-charset*))

(defparameter *html-lang* "en")

(defparameter *html-charset* "UTF-8")

(defun make-html (&rest args)
  `(:html :lang *html-lang*
          ,@args))

(defun make-head (&rest args)
  `(:head
    (:meta :charset *html-charset*)
    ,@args))

(defun write-raw (&rest args)
  `(prog1 nil ,@(loop for arg in args
                      collect `(fill-text ,arg t))))
