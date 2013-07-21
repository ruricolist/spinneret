;;;; Functions used at run time or compile time.

(in-package #:spinneret)

(declaim (type (integer -1 #.most-positive-fixnum) *depth*))

(defvar *depth* -1
  "Depth of the tag being output.")

(defvar *pre* nil)

(defun indent ()
  (when *print-pretty*
   (format *html* "~V,0T" *depth*)))

(defun newline-and-indent ()
  "Fresh line and indent according to *DEPTH*."
  (when *print-pretty*
   (format *html* "~&~V,0T" *depth*)))

(defun emit-end-tag (tag)
  (if *print-pretty*
      (format *html*
              "~V,0T~:*~<~%~V,0T~1,V:;~A~>"
              *depth*
              *print-right-margin*
              tag)
      (write-string tag *html*)))

(defmacro without-trailing-space (&body body)
  `(let ((*pending-space* nil))
     ,@body))

(declaim (boolean *pending-space*))

(defvar *pending-space* nil)

(defun buffer-space ()
  (setf *pending-space* t))

(defun flush-space ()
  (when (shiftf *pending-space* nil)
    (write-char #\Space *html*)))

(declaim (inline buffer-space flush-space))

(defmacro catch-output (arg)
  (typecase arg
    (null nil)
    (string `(fill-text ,(escape-string arg) t))
    ((or character number)              ;not symbol, because not evaluated.
     `(fill-text ,(escape-string (princ-to-string arg)) t))
    (t `(html ,arg))))

(defgeneric html (object)
  (:method (object) nil)
  (:method ((nada null)) nil)
  (:documentation "Return an unescaped, unfilled string representing OBJECT."))

(defmethod html :around (object)
  (when-let (object (call-next-method))
    (fill-text (escape-string object) t)))

(defmethod html ((string string))
  string)

(defmethod html ((char character))
  (princ-to-string char))

(defmethod html ((n number))
  (princ-to-string n))

(defmethod html ((sym symbol))
  (princ-to-string sym))

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

(defun fill-text (string &optional safe?)
  (declare (string string))
  (if (and *print-pretty* (not *pre*))
      (progn
        (format *html* "~V,0T" *depth*)
        (do-words (word string)
          (flush-space)
          (format *html* "~<~%~V,0T~1,V:;~A~>"
                  *depth*
                  *print-right-margin*
                  (if safe? word (escape-string word)))
          (buffer-space)))
      (if *pre*
          (format *html* "~&~A~%" string)
          (progn
            (flush-space)
            (write-string string *html*)
            (buffer-space)))))

(defun pop-word (stream-in stream-out)
  (declare (optimize speed)
           (stream stream-in stream-out))
  (loop initially (peek-char t stream-in nil)
        for c = (read-char stream-in nil)
        while (and c (not (whitespace c)))
        do (write-char c stream-out)
        finally (return (get-output-stream-string stream-out))))

(defun format-attributes (attrs)
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
                     (format *html* "~( ~A~)~:_" attr)
                     (format *html* "~( ~A~)~:_=~:_~A~:_"
                             attr
                             (if (equal value "")
                                 "\"\""
                                 value)))))
             (inner (attrs)
               (declare (optimize speed))
               (loop (unless attrs (return))
                     (pprint-indent :block 1 *html*)
                     (let ((attr (pop attrs))
                           (value (pop attrs)))
                       (declare (symbol attr))
                       (if (eql attr :attrs)
                           (loop for (a v . rest) on value by #'cddr
                                 do (format-attr a (escape-value v)))
                           (format-attr attr value))))))
      (declare (inline seen? inner))
      (if *print-pretty*
          (pprint-logical-block (*html* nil :suffix ">")
            (inner attrs))
          (progn (inner attrs)
                 (write-char #\> *html*))))))

(defun escape-value (value)
  (if (member value '(t nil) :test #'eq)
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
  (when *print-pretty*
    (terpri *html*)))

(defun xss-escape (arg)
  "Possibly escape ARG for use with FORMAT.

We don't want to leave ourselves open to XSS, but we also want to be
able to use directives like ~c, ~d, ~{~} &c."
  (typecase arg
    ((or number character symbol)
     arg)
    (list
     (mapcar #'escape-unsafe arg))
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
