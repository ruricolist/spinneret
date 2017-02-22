(in-package #:spinneret)

(declaim (inline whitespace must-quote?
                 escape-string escape-attribute-value
                 escape-cdata escape-comment))

(deftype index () '(integer 0 #.array-total-size-limit))

;; See 2.5.1.
;; http://www.w3.org/TR/html5/common-microsyntaxes.html#space-character

(declaim (inline whitespace))
(defun whitespace (char)
  (declare (character char))
  (case char
    ((#\Space #\Tab #\Newline #\Page #\Return) t)))

;; See 8.1.2.3.
;; http://www.w3.org/TR/html5/syntax.html#syntax-attribute-value

(defun must-quote? (char)
  (declare (character char))
  (or (whitespace char)
      (case char
        ((#\" #\' #\` #\= #\< #\>) t))))

(defun needs-quotes? (string)
  (declare (string string))
  (or (some #'must-quote? string)
      (ends-with #\/ string)))

;; See 8.3.
;; http://www.w3.org/TR/html5/the-end.html#serializing-html-fragments

(defun escape-string-char (c)
  (case c
    (#\& "&amp;")
    (#\No-break_space "&nbsp;")
    (#\< "&lt;")
    (#\> "&gt")))

(defun escape-string (string)
  (escape-with-table string #'escape-string-char))

(defun escape-to-string (object)
  (if (stringp object)
      (escape-string object)
      (escape-string (princ-to-string object))))

(defun escape-attribute-value (string)
  (escape-with-table string
                     (lambda (c)
                       (case c
                         (#\& "&amp;")
                         (#\No-break_space "&nbsp;")
                         (#\" "&quot;")))))

(defun escape-to-stream (string table stream)
  (declare (stream stream)
           (optimize speed))
  (let ((start-pointer 0)
        (end-pointer 0)
        (rep (ensure-function
              (etypecase table
                (function table)
                (hash-table (lambda (c)
                              (gethash c table)))))))
    (declare (index start-pointer)
             ((or null index) end-pointer))
    (loop (setf end-pointer
                (position-if rep string :start start-pointer))
          (if end-pointer
              (progn
                (write-string string stream
                              :start start-pointer
                              :end end-pointer)
                (write-string
                 (funcall rep (char string end-pointer))
                 stream)
                (setf start-pointer (1+ end-pointer)))
              (progn
                (write-string string stream :start start-pointer)
                (return))))))

(defun escape-with-table (string table)
  (with-output-to-string (s)
    (escape-to-stream string table s)))

;; See 8.1.5
;; http://www.w3.org/TR/html5/syntax.html#cdata-sections

(defparameter *cdata-start* "<![CDATA[")

(defparameter *cdata-end* "]]>")

(defun escape-cdata (text)
  (remove-substring text *cdata-end*))

;; See 8.1.6
;; http://www.w3.org/TR/html5/syntax.html#comments

(defun escape-comment (text)
  (remove-substring (string-trim ">-" text) "--"))

(defun remove-substring (string substring)
  (with-output-to-string (s)
    (let ((len (length substring)))
      (loop for start = 0 then (+ end len)
            for end = (search substring string :start2 start)
            do (write-string string s :start start :end end)
            while end))))
