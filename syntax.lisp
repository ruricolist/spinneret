(in-package #:spinneret)

(declaim (inline whitespace must-quote?
                 escape-string escape-attribute-value
                 escape-cdata escape-comment))

(declaim (hash-table *whitespace* *unsafe*
                     *string-escapes* *attribute-value-escapes*))

(deftype index () '(integer 0 #.array-total-size-limit))

;; See 2.5.1.
;; http://www.w3.org/TR/html5/common-microsyntaxes.html#space-character

(defparameter *whitespace*
  (let ((ht (make-hash-table)))
    (dolist (c '(#\Space #\Tab #\Newline #\Page #\Return))
      (setf (gethash c ht) t))
    ht))

(defun whitespace (char)
  (declare (character char))
  (nth-value 1 (gethash char *whitespace*)))

;; See 8.1.2.3.
;; http://www.w3.org/TR/html5/syntax.html#syntax-attribute-value

(defparameter *unsafe*
  (let ((ht (make-hash-table)))
    (dolist (c '(#\" #\' #\` #\= #\< #\>))
      (setf (gethash c ht) t))
    ht))

(defun needs-quotes? (string)
  (declare (string string))
  (or (some #'must-quote? string)
      (ends-with #\/ string)))

(defun must-quote? (char)
  (declare (character char))
  (or (nth-value 1 (gethash char *whitespace*))
      (nth-value 1 (gethash char *unsafe*))))

;; See 8.3.
;; http://www.w3.org/TR/html5/the-end.html#serializing-html-fragments

(defparameter *string-escapes*
  (let ((ht (make-hash-table)))
    (setf (gethash #\& ht) "&amp;"
          (gethash #\No-break_space ht) "&nbsp;"
          (gethash #\< ht) "&lt;"
          (gethash #\> ht) "&gt;")
    ht))

(defun escape-string (string)
  (escape-with-table string *string-escapes*))

(defun escape-to-string (object)
  (if (stringp object)
      (escape-string object)
      (escape-string (princ-to-string object))))

(defparameter *attribute-value-escapes*
  (let ((ht (make-hash-table)))
    (setf (gethash #\& ht) "&amp;"
          (gethash #\No-break_space ht) "&nsbp;"
          (gethash #\" ht) "&quot;")
    ht))

(defun escape-attribute-value (string)
  (escape-with-table string *attribute-value-escapes*))

(defun escape-to-stream (string table stream)
  (declare (hash-table table)
           (string-stream stream)
           (optimize speed))
  (let ((start-pointer 0)
        (end-pointer 0))
    (declare (index start-pointer)
             ((or null index) end-pointer))
    (loop (setf end-pointer
                (position-if (lambda (c) (gethash c table)) string :start start-pointer))
          (if end-pointer
              (progn
                (write-string string stream
                              :start start-pointer
                              :end end-pointer)
                (write-string
                 (gethash (char string end-pointer) table)
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

(memoize-weakly
 (defun escape-cdata (text)
   (remove-substring text *cdata-end*)))

;; See 8.1.6
;; http://www.w3.org/TR/html5/syntax.html#comments

(memoize-weakly
 (defun escape-comment (text)
   (remove-substring (string-trim ">-" text) "--")))

(defun remove-substring (string substring)
  (declare (string string substring))
  (with-output-to-string (s)
    (let ((len (length substring)))
      (labels ((rec (string start)
                 (let ((end (search substring string :start2 start)))
                   (write-string string s :start start :end end)
                   (when end
                     (rec string (+ end len))
                     string))))
        (rec string 0)))))
