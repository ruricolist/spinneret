;;;; Functions used at run time or compile time.

(in-package #:spinneret)

(declaim (type (integer -1 #.most-positive-fixnum) *depth*))

(defvar *depth* -1
  "Depth of the tag being output.")

(defvar *pre* nil)

(defun indent ()
  (when *print-pretty*
   (format *html* "~V,0T" *depth*)))

(defun newline-and-indent (&optional (stream *html*))
  "Fresh line and indent according to *DEPTH*."
  (format stream "~&~V,0T" *depth*))

(defun emit-pretty-end-tag (tag &optional (stream *html*))
  (format stream
          "~V,0T~:*~<~%~V,0T~1,V:;~A~>"
          *depth*
          *print-right-margin*
          tag))

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
    nil)
  (:method ((nada null)) nil)
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
  (format *html* "~d" n))

(defmethod html ((sym symbol))
  (format *html* "~a" sym))

(defun mklist (x) (if (listp x) x (list x)))

(defmacro do-words ((var string) &body body)
  (let ((stream (gensym)) (word (gensym)))
    `(let ((,stream (make-string-input-stream ,string))
           (,word (make-string-output-stream)))
       (declare (stream ,stream ,word))
       (loop (let ((,var (pop-word ,stream ,word)))
               (if (equal ,var "")
                   (return)
                   (progn ,@body)))))))

(declaim (inline pop-word))

(defun pop-word (stream-in stream-out)
  (declare (optimize speed)
           (stream stream-in stream-out))
  (loop initially (peek-char t stream-in nil)
        for c = (read-char stream-in nil)
        while (and c (not (whitespace c)))
        do (write-char c stream-out)
        finally (return (get-output-stream-string stream-out))))

(defun fill-text (string &optional safe?)
  (check-type string string)
  (if (and *print-pretty* (not *pre*))
      (progn
        (format *html* "~V,0T" *depth*)
        (do-words (word string)
          (with-space
            (format *html* "~<~%~V,0T~1,V:;~A~>"
                    *depth*
                    *print-right-margin*
                    (if safe? word (escape-string word))))))
      (if *pre*
          (format *html* "~&~A~%" string)
          (with-space
            (if safe?
                (write-string string *html*)
                (escape-to-stream string #'escape-string-char *html*)))))
  (values))

(defun format-attributes (attrs &optional (stream *html*))
  (declare (stream stream))
  (let ((seen '()))
    ;; Ensure that the leftmost keyword has priority,
    ;; as in function lambda lists.
    (labels ((seen? (name)
               (declare (optimize speed)
                        (symbol name))
               (prog1 (member name seen)
                 (push name seen)))
             (format-attr (attr value)
               (declare (optimize speed))
               (unless (or (seen? attr) (null value))
                 (if (boolean? attr)
                     (format stream "~( ~A~)~:_" attr)
                     (format stream "~( ~A~)~:_=~:_~A~:_"
                             attr
                             (cond ((equal value "") "\"\"")
                                   ((keywordp value) (string-downcase value))
                                   ((eql value t) "true")
                                   (t value))))))
             (inner (attrs)
               (declare (optimize speed))
               (loop (unless attrs (return))
                     (pprint-indent :block 1 stream)
                     (let ((attr (pop attrs))
                           (value (pop attrs)))
                       (declare (symbol attr))
                       (if (eql attr :attrs)
                           (loop for (a v . nil) on value by #'cddr
                                 do (format-attr a (escape-value v)))
                           (format-attr attr value))))))
      (declare (inline seen? inner))
      (if *print-pretty*
          (pprint-logical-block (stream nil :suffix ">")
            (inner attrs))
          (progn (inner attrs)
                 (write-char #\> stream))))))

(defun escape-value (value)
  (if (or (eq value t)
          (eq value nil)
          (keywordp value))
      value
      (let ((string (escape-attribute-value
                     (princ-to-string value))))
        (if (needs-quotes? string)
            (format nil "\"~A\"" string)
            string))))

(defun format-text (control-string &rest args)
  (when *print-pretty*
    (fresh-line *html*))
  (let ((*depth* (1+ *depth*)))
    (fill-text (apply #'format nil control-string args) t))
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
  (write-string "<!DOCTYPE html>" *html*)
  (when *print-pretty*
    (terpri *html*)))

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

(defun cdata (text safe?)
  (write-string *cdata-start* *html*)
  (write-string (if safe?
                    text
                    (escape-cdata text))
                *html*)
  (write-string *cdata-end* *html*))

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
