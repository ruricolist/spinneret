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
    (setf last-char char)
    char)

  (:method stream-write-string (s string &optional (start 0) end)
    (let ((end (or end (length string)))
          (start (or start 0)))
      (declare (type array-index start end))
      (nlet rec ((start start))
        (when (nix elastic-newline)
          (unless (serapeum:string^= #\Newline string)
            (incf line)
            (setf col 0)
            (terpri base-stream)))
        (let ((newline-count (count #\Newline string :start start :end end)))
          (if (= newline-count 0)
              (progn
                (write-string string base-stream :start start :end end)
                (incf col (- end start)))
              (progn
                (incf line newline-count)
                (setf col (- end (position #\Newline string
                                           :from-end t
                                           :end end
                                           :start start))))))
        (when (> (- end start) 1)
          (setf last-char (aref string (1- end))))))
    string)

  (:method stream-terpri (s)
    (incf line)
    (setf col 0)
    (setf last-char #\Newline)
    (terpri base-stream))

  (:method stream-fresh-line (s)
    (unless (eql last-char #\Newline)
      (terpri s)))

  (:method stream-finish-output (s)
    (finish-output base-stream))

  (:method stream-force-output (s)
    (force-output base-stream))

  (:method stream-advance-to-column (s c)
    (when (< col c)
      (loop repeat (- c col) do
        (write-char #\Space s)))
    t)

  (:method elastic-newline (s)
    (setf elastic-newline t)))


(defmacro with-block ((&key (stream '*html*)) &body body)
  (serapeum:with-thunk (body)
    `(if *print-pretty*
         (let ((*block-start* (html-stream-column ,stream)))
           (funcall ,body))
         (funcall ,body))))
