;;;; Functions used at run time or compile time.

(in-package #:spinneret)

(declaim (type (integer -1 #.most-positive-fixnum) *depth*))

(defvar *depth* -1
  "Depth of the tag being output.")

(defvar *pre* nil)

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
                            :displaced-index-offset 0)))
    (loop with len = (length string)
          for left = 0 then (+ right 1)
          for right = (or (position-if #'whitespace string :start left) len)
          unless (= left right)
            do (adjust-array window (- right left)
                             :displaced-to string
                             :displaced-index-offset left)
               (funcall thunk window)
          until (>= right len))))

(defmacro do-words ((var string) &body body)
  `(call/words (lambda (,var) ,@body)
               ,string))

(defun fill-text (string &optional safe?)
  (check-type string string)
  (cond (*pre*
         (fast-format *html* "~&~A~%" string))
        (*print-pretty*
         (let ((html *html*)
               (depth *depth*))
           (format html "~V,0T" depth)
           (pprint-newline :fill html)
           (do-words (word string)
             (let ((word (if safe? word (escape-string word))))
               (write-string word html))
             ;; This will discard the preceding space if necessary.
             (write-char #\Space html)
             (pprint-newline :fill html))))
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

(defun format-attributes/inline (attrs &optional (stream *html*))
  (declare (stream stream))
  (if (null attrs)
      (write-char #\> stream)
      (let ((seen '()))
        ;; Ensure that the leftmost keyword has priority,
        ;; as in function lambda lists.
        (write-char #\Space stream)
        (labels ((seen? (name)
                   (declare (optimize speed)
                            (symbol name))
                   (prog1 (member name seen)
                     (push name seen)))
                 (format-attr (attr value)
                   (unless (or (null value) (seen? attr))
                     (if (boolean? attr)
                         (progn
                           (pprint-newline :fill stream)
                           (format stream "~(~a~)" attr))
                         (let ((value (format-attribute-value value)))
                           (pprint-newline :fill stream)
                           (format stream "~(~a~)=" attr)
                           (pprint-newline :fill stream)
                           (format stream "~a" value)))))
                 (dynamic-attrs (attrs)
                   (loop for (a v . rest) on attrs by #'cddr
                         do (format-attr a (escape-value v))
                            (when rest
                              (write-char #\Space stream)))))
          (declare (inline seen?))
          (loop for attr = (pop attrs)
                for value = (pop attrs)
                if (eql attr :attrs)
                  do (dynamic-attrs value)
                else do (format-attr attr value)
                        (when (null attrs)
                          (loop-finish))
                        (write-char #\Space stream)
                        (pprint-newline :fill stream))))))

(defun format-attributes (attrs &optional (stream *html*))
  (declare (stream stream))
  (if (null attrs)
      (write-char #\> stream)
      (let ((seen '())
            *print-lines* *print-miser-width*
            *print-level* *print-length*)
        ;; Ensure that the leftmost keyword has priority,
        ;; as in function lambda lists.
        (write-char #\Space stream)
        (pprint-logical-block (stream attrs)
          (labels ((seen? (name)
                     (declare (optimize speed)
                              (symbol name))
                     (prog1 (member name seen)
                       (push name seen)))
                   (format-attr (attr value)
                     (unless (or (null value) (seen? attr))
                       (if (boolean? attr)
                           (pprint-logical-block (stream nil)
                             (format stream "~(~a~)" attr))
                           (let ((value (format-attribute-value value)))
                             (pprint-logical-block (stream nil)
                               (format stream "~(~a~)=" attr)
                               (pprint-newline :fill stream)
                               (format stream "~a" value))))))
                   (dynamic-attrs (attrs)
                     (loop for (a v . rest) on attrs by #'cddr
                           do (format-attr a (escape-value v))
                              (when rest
                                (write-char #\Space stream)))))
            (declare (inline seen?))
            (loop for attr = (pprint-pop)
                  for value = (pprint-pop)
                  if (eql attr :attrs)
                    do (dynamic-attrs value)
                  else do (format-attr attr value)
                          (pprint-exit-if-list-exhausted)
                          (write-char #\Space stream)
                          (pprint-newline :linear stream)))))))

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
  (pprint-newline :mandatory *html*)
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
