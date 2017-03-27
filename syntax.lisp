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
      (serapeum:string$= "/" string)))

;; See 8.3.
;; http://www.w3.org/TR/html5/the-end.html#serializing-html-fragments

(defun escape-string-char (c)
  (declare (character c)
           (optimize (speed 3) (safety 1) (debug 0)))
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
  (serapeum:escape string table :stream stream))

(defun escape-with-table (string table)
  (serapeum:escape string table))

;; See 8.1.5
;; http://www.w3.org/TR/html5/syntax.html#cdata-sections

(serapeum:defconst cdata-start "<![CDATA[")

(serapeum:defconst cdata-end "]]>")

(defun escape-cdata (text)
  (remove-substring text cdata-end))

;; See 8.1.6
;; http://www.w3.org/TR/html5/syntax.html#comments

(defun escape-comment (text)
  (remove-substring (string-trim ">-" text) "--"))

(defun remove-substring (string substring)
  (serapeum:string-replace-all substring string ""))
