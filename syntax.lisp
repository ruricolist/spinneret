(in-package #:spinneret)

(declaim (inline whitespace must-quote?
                 escape-string escape-attribute-value
                 escape-cdata escape-comment))

(deftype index () '(integer 0 #.array-total-size-limit))

(defconst no-break-space
  #+lispworks #\No-break-space
  #-lispworks #\No-break_space)

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
  (or *always-quote*
      (some #'must-quote? string)
      (string$= "/" string)))

;; See 8.3.
;; http://www.w3.org/TR/html5/the-end.html#serializing-html-fragments

(defun escape-string-char (c)
  (declare (character c)
           (optimize (speed 3) (safety 1) (debug 0)))
  (case c
    (#\& "&amp;")
    (#.no-break-space "&nbsp;")
    (#\< "&lt;")
    (#\> "&gt;")
    (#\" "&quot;")
    (#\' "&#39;")))

(defun escape-string (string)
  "Escape STRING as HTML."
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
                         (#.no-break-space "&nbsp;")
                         (#\" "&quot;")
                         (#\' "&#39;")))))

(defun escape-to-stream (string table stream)
  (escape string table :stream stream))

(defun escape-with-table (string table)
  (escape string table))

;; See 8.1.5
;; http://www.w3.org/TR/html5/syntax.html#cdata-sections

(defconst cdata-start "<![CDATA[")

(defconst cdata-end "]]>")

(defun escape-cdata (text)
  (remove-substring text cdata-end))

;; See 8.1.6
;; http://www.w3.org/TR/html5/syntax.html#comments

(defun escape-comment (text)
  (remove-substring (string-trim ">-" text) "--"))

(defun remove-substring (string substring)
  (string-replace-all substring string ""))
