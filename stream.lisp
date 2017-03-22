(in-package :spinneret)

(defclass html-stream (fundamental-character-output-stream)
  ((col :type (integer 0 *) :initform 0
        :reader html-stream-column)
   (line :type (integer 0 *) :initform 0)
   (last-char :type character
              ;; The last char defaults to newline to get reasonable
              ;; behavior from fresh-line.
              :initform #\Newline)
   (elastic-newline :type boolean
                    :initform nil)
   (base-stream :type stream
                :initarg :base-stream)))

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

(defgeneric html-stream-column (stream)
  (:method ((x stream))
    0))

(serapeum:defmethods html-stream (s col line last-char base-stream
                                    elastic-newline)
  (:method ensure-html-stream (s)
    s)

  (:method html-stream? (s)
    t)

  (:method stream-line-column (s)
    col)

  (:method stream-start-line-p (s)
    (= col 0))

  (:method stream-write-char (s (char (eql #\Newline)))
    (nix elastic-newline)
    (incf line)
    (setf col 0)
    (setf last-char #\Newline)
    (terpri base-stream)
    #\Newline)

  (:method stream-write-char (s char)
    (when (nix elastic-newline)
      (write-char #\Newline s))
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
               (when (nix elastic-newline)
                 (unless (eql (aref string start) #\Newline)
                   (incf line)
                   (setf col 0)
                   (terpri base-stream)))
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
    (incf line)
    (setf col 0)
    (setf last-char #\Newline)
    (nix elastic-newline)
    (terpri base-stream))

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


(defmacro with-block ((&key (stream '*html*)
                            (offset 0))
                      &body body)
  (serapeum:with-thunk (body)
    `(let ((*block-start*
             (+ (html-stream-column ,stream)
                ,offset)))
       (funcall ,body))))
