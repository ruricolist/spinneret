;;;; Functions used at run time or compile time.

(in-package #:spinneret)

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

(defmacro without-trailing-space (&body body)
  `(let ((*pending-space* nil))
     ,@body))

(defmacro with-space (&body body)
  `(progn
     (flush-space)
     ,@body
     (buffer-space)))

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

(defun maybe-wrap (&optional (offset 0) (stream *html*))
  (when *print-pretty*
    (let* ((indent (get-indent))
           (fill *fill-column*)
           (goal (+ fill indent))
           (col (+ offset (html-stream-column stream))))
      (when (> col goal)
        (terpri stream)))))

(defun fill-text (string &optional safe?
                  &aux (html *html*)
                       (pretty? *print-pretty*)
                       (pre? *pre*))
  (check-type string string)
  (cond
    ((= (length string) 0))
    (pre?
     (let ((stream (html-stream.base-stream html)))
       (write-string string stream)))
    (pretty?
     (let* ((start-col (get-indent))
            (fill *fill-column*)
            (goal (+ fill start-col)))
       (when (eql *html-style* :tree)
         (fresh-line html))
       (when (whitespace (aref string 0))
         (write-char #\Space html))
       (flet ((wrap () (terpri html))) (declare (inline wrap))
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

(defgeneric html-length (x)
  (:documentation "The length of X when printed as an HTML string.

This is provided so you can give Spinneret the information it needs to
make reasonable decisions about line wrapping.")
  (:method ((x t)) 0))

(defun html-length* (x)
  (typecase x
    ((eql t) 4)
    (string (length x))
    (symbol (length (symbol-name x)))
    (character 1)
    (integer
     (eif (zerop x) 1
          (let ((x (abs x))
                ;; Single precision is not enough.
                (base (coerce *print-base* 'double-float)))
            (1+ (floor (log x base))))))
    (otherwise
     (assure unsigned-byte (html-length x)))))

(defun format-attributes-pretty/inline (attrs &optional (stream *html*))
  (declare (stream stream))
  (let* ((start-col (get-indent))
         (fill *fill-column*)
         (goal (+ start-col fill)))
    (serapeum:fbind* ((too-long?
                       (if *print-pretty*
                           (lambda (len)
                             (> (+ len (html-stream-column stream))
                                goal))
                           (constantly nil)))
                      (print-prefix
                       (lambda (len attr)
                         (let ((prefix (if (too-long? len) #\Newline #\Space)))
                           (write-char prefix stream)
                           ;; XXX Work around
                           ;; <https://abcl.org/trac/ticket/166>
                           #+abcl (write-string (string-downcase attr) stream)
                           #-abcl (format stream "~(~a~)" attr))))
                      (print-boolean
                       (lambda (attr)
                         (let ((len (length (symbol-name attr))))
                           ;; No valid attribute is longer than 80. (I
                           ;; suppose a data attribute could be.)
                           (print-prefix len attr))))
                      (print-attr
                       (lambda (attr value)
                         (let ((len (+ (length (symbol-name attr))
                                       1 ;for the equals sign
                                       (html-length* value))))
                           (print-prefix len attr))
                         (write-char #\= stream)
                         (format stream "~a" value))))
      (declare (dynamic-extent #'print-prefix #'print-boolean #'print-attr))
      (format-attributes-with attrs
                              #'print-boolean
                              #'print-attr))))

(defun format-attributes-pretty/block (attrs &optional (stream *html*))
  (declare (html-stream stream))
  (let ((*fill-column* (truncate *fill-column* 2))
        ;; Force the attributes to line up.
        (*indent* (1+ (html-stream-column stream))))
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
  (fill-text (format nil "~?" control-string args) t)
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

(-> heading-depth () (integer 1 6))
(defun heading-depth ()
  "Return the current dynamic heading depth.
This follows the convention for the XHTML <h/> element, where the top
level is level 1, inside the first section is level 2, and so forth."
  (clamp (count :section *html-path*) 1 6))

(defun heading-depth-heading ()
  (ecase (heading-depth)
    (1 :h1)
    (2 :h2)
    (3 :h3)
    (4 :h4)
    (5 :h5)
    (6 :h6)))
