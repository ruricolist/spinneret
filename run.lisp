;;;; Functions used at run time or compile time.

(in-package #:spinneret)

(declaim (type (integer -1 #.most-positive-fixnum) *depth*)
         (fixnum *html-fill-column* *html-min-room*))

(defvar *depth* -1
  "Depth of the tag being output.")

(defparameter *html-fill-column* 80
  "Width of HTML output.")

(defparameter *html-min-room* 30
  "Room (in columns) required to justify text. With less room than
  this, move back to column 0 before justifying.")

(defun indent ()
  (let (*print-pretty*)
    (format *html* "~V,0T" *depth*)))

(defun newline-and-indent ()
  "Fresh line and indent according to *DEPTH*."
  (let (*print-pretty*)
    (format *html* "~&~V,0T" *depth*)))

(defun justify-end-tag (tag)
  (let (*print-pretty*)
    (format *html*
            "~V,0T~:*~<~%~V,0T~1,V:;~A~>"
            *depth*
            *html-fill-column*
            tag)))

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

(defun catch-string (arg pre?)
  (when arg
    (cond ((stringp arg)
           (fill-text arg pre?))
          ((or (characterp arg)
               (numberp arg)
               (symbolp arg))
           (fill-text (princ-to-string arg) pre?)))))

(defun fill-text (text &optional pre?)
  (declare (string text) (boolean pre?))
  (cond ((every #'whitespace text)
         (write-string text *html*))
        (pre?
         (let (*print-pretty*)
           (format *html* "~&~A~%" text)))
        (t (justify text))))

(defmacro do-words ((var string) &body body)
  (let ((stream (gensym)) (word (gensym)))
    `(let ((,stream (make-string-input-stream ,string))
           (,word (make-string-output-stream)))
       (declare (stream ,stream ,word))
       (loop (let ((,var (pop-word ,stream ,word)))
               (if (string= ,var "")
                   (return)
                   (progn ,@body)))))))

(defun justify (text)
  (declare (string text))
  (let ((room? (when (< *depth* *html-fill-column*)
                 (let ((space (- *html-fill-column* *depth*)))
                   (> space *html-min-room*)))))
    (unless room? (fresh-line *html*))
    (let ((*depth* (if room? *depth* 0))
          *print-pretty*)
      (format *html* "~V,0T" *depth*)
      (do-words (word text)
        (flush-space)
        (format *html* "~<~%~V,0T~1,V:;~A~>"
                *depth*
                *html-fill-column*
                word)
        (buffer-space)))))

(defun pop-word (stream-in stream-out)
  (declare (optimize speed)
           (stream stream-in stream-out))
  (loop initially (peek-char t stream-in nil)
        for c = (read-char stream-in nil)
        while (and c (not (whitespace c)))
        do (write-char c stream-out)
        finally (return (get-output-stream-string stream-out))))

(defun format-attributes (&rest attrs)
  (let ((seen (make-hash-table)))
    ;; Ensure that the leftmost keyword has priority,
    ;; as in function lambda lists.
    (labels ((seen? (name)
               (shiftf (gethash name seen nil) t))
             (format-attr (attr value)
               (unless (or (seen? attr) (null value))
                 (if (boolean? attr)
                     (format *html* "~( ~A~)~:_" attr)
                     (format *html* "~( ~A~)~:_=~:_~A~:_"
                             attr
                             (if (equal value "")
                                 "\"\""
                                 value))))))
      (declare (inline seen?))
      (pprint-logical-block (*html* nil :suffix ">")
        (declare (optimize speed))
        (loop (unless attrs (return))
              (pprint-indent :block 1 *html*)
              (let ((attr (pop attrs))
                    (value (pop attrs)))
                (declare (symbol attr))
                (if (eql attr :dataset)
                    (loop for (key val . rest) on value by #'cddr
                          do (format-attr (make-keyword "data-" key)
                                          (escape-value val)))
                    (format-attr attr value))))))))

(defun make-keyword (&rest parts)
  (intern (string-upcase (format nil "~{~A~}" parts)) :keyword))

(defun escape-value (value)
  (if (member value '(t nil) :test #'eq)
      value
      (let ((string (escape-attribute-value
                     (if (stringp value)
                         value
                         (princ-to-string value)))))
        (if (needs-quotes? string)
            (format nil "\"~A\"" string)
            string))))

(defun format-text (control-string &rest args)
  (fresh-line *html*)
  (let (*print-pretty*
        (*depth* (+ *depth* 1)))
    (justify
     (apply #'format nil control-string args)))
  (terpri *html*))

(defun make-doctype (&rest args)
  (declare (ignore args))
  `(doctype))

(defun doctype (&rest args)
  (declare (ignore args))
  (write-string "<!DOCTYPE html>" *html*)
  (terpri *html*))

(defun make-comment (text)
  `(comment ,(if (stringp text)
                 (escape-comment text)
                 text)
            ,(stringp text)))

(defun comment (text safe?)
  (let (*print-pretty*
        (*depth* (+ *depth* 1)))
    (format *html* "~&~v,0T<!-- " *depth*)
    (justify (if safe?
                 text
                 (escape-comment text)))
    (format *html* " -->~%")))

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

(defvar *invalid* nil)

(defparameter *check-tags* nil
  "Whether to print warnings at runtime for invalid HTML tags. Not to
  be confused with HTML validation.")

(defun note-invalid (element)
  (when *check-tags*
    (push (cons *depth* element) *invalid*)))

(defun maybe-report-invalid-elements ()
  (when *check-tags*
    (let ((stack (nreverse (shiftf *invalid* nil))))
      (loop
        (unless stack (return))
        (destructuring-bind (depth . element)
            (pop stack)
          (if (embedded? element)
              (loop while (and stack (< depth (caar stack)))
                    do (pop stack))
              (warn "~&~S is not a valid element in HTML5." element)))))))
