(in-package :spinneret)

(defgeneric html-stream.base-stream (stream)
  (:method ((stream stream))
    stream))

(defgeneric html-stream-column (stream)
  (:method ((x stream))
    0))

(defclass html-stream (fundamental-character-output-stream)
  ((col :type (integer 0 *) :initform 0
        :reader html-stream-column
        :reader stream-line-column)
   (line :type (integer 0 *) :initform 0)
   (last-char :type character
              ;; The last char defaults to newline to get reasonable
              ;; behavior from fresh-line.
              :initform #\Newline)
   (elastic-newline :type boolean
                    :initform nil)
   (base-stream :type stream
                :initarg :base-stream
                :reader html-stream.base-stream))
  (:default-initargs))

(defun make-html-stream (base-stream)
  (make-instance 'html-stream
                 :base-stream (assure stream base-stream)))

(defgeneric html-stream? (x)
  (:method ((x t)) nil))

(defgeneric ensure-html-stream (x)
  (:method ((x stream))
    (if *print-pretty*
        (make-html-stream x)
        (assure stream x))))

(defgeneric elastic-newline (stream)
  (:method ((x t))
    (values)))

(defun newline (&optional s)
  (when *print-pretty*
    (terpri s)))

(serapeum:defmethods html-stream (s col line last-char base-stream
                                    elastic-newline)
  (:method ensure-html-stream (s)
    s)

  (:method html-stream? (s)
    t)

  (:method stream-start-line-p (s)
    (= col 0))

  (:method fire-elastic-newline (s (char (eql #\Newline)))
    (nix elastic-newline))

  (:method fire-elastic-newline (s (char character))
    (when (nix elastic-newline)
      (unless *pre*
        (write-char #\Newline s))))

  (:method stream-write-char (s (char (eql #\Newline)))
    (nix elastic-newline)
    ;; Remember the starting value is -1.
    (let ((indent (max 0 (get-indent))))
      (write-char #\Newline base-stream)
      (incf line)
      ;; (PRINC INDENT)
      (setf col indent)
      (loop repeat indent do
        (write-char #\Space base-stream)))
    (setf last-char #\Newline))

  (:method stream-write-char (s char)
    (fire-elastic-newline s char)
    (incf col 1)
    (write-char char base-stream)
    (setf last-char char))

  (:method stream-write-string (s string &optional (start 0) end)
    (declare (type (or null array-index) start end))
    (prog1 string
      (let* ((end (or end (length string)))
             (start (or start 0))
             (len (assure array-index (- end start))))
        (cond ((= len 0))
              ((= len 1)
               (write-char (aref string start) s))
              (t
               (fire-elastic-newline s (aref string start))
               (setf last-char (aref string (1- end)))
               (multiple-value-bind (newline-count chars)
                   (nlet rec ((i start)
                              (lines 0)
                              (chars 0))
                     (eif (= i end)
                          (values lines chars)
                          (let ((c (aref string i)))
                            (eif (eql c #\Newline)
                                 (rec (1+ i)
                                      (1+ lines)
                                      0)
                                 (rec (1+ i)
                                      lines
                                      (1+ chars))))))
                 (declare (array-index newline-count chars))
                 (write-string string base-stream :start start :end end)
                 (eif (> newline-count 0)
                      (progn
                        (incf line newline-count)
                        (setf col chars))
                      (incf col chars))))))))

  (:method stream-terpri (s)
    (write-char #\Newline s))

  (:method stream-fresh-line (s)
    (prog1 (unless (eql last-char #\Newline)
             (terpri s))
      (assert (eql last-char #\Newline))))

  (:method stream-finish-output (s)
    (finish-output base-stream))

  (:method stream-force-output (s)
    (force-output base-stream))

  (:method stream-advance-to-column (s c)
    (loop while (< col c) do
      (write-char #\Space s))
    (assert (>= col c))
    t)

  (:method elastic-newline (s)
    (setf elastic-newline t)))
