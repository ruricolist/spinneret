(in-package #:spinneret)

(defun parse-deftag-body (body)
  (multiple-value-bind (name attrs body)
      (tag-parts (cons :tag body))
    (declare (ignore name))
    (multiple-value-bind (body decls)
        (parse-body body)
      (values body attrs decls))))

(defun splice-allow-other-keys (lambda-list)
  (let ((keys (member '&key lambda-list)))
    (if (null keys)
        lambda-list
        (let ((end-of-keys (member-if (lambda (x)
                                        (and (symbolp x)
                                             (starts-with-subseq "&" (string x))))
                                      (cdr keys))))
          (append (ldiff lambda-list keys)
                  (ldiff keys end-of-keys)
                  '(&allow-other-keys)
                  end-of-keys)))))

(defun allow-other-keys (lambda-list)
  (cond ((null lambda-list)
         '(&key &allow-other-keys))
        ((find '&allow-other-keys lambda-list)
         lambda-list)
        (t (splice-allow-other-keys lambda-list))))

(defun extract-lambda-list-keywords (lambda-list)
  "Get the actual keywords from the lambda list."
  (mapcar #'caar (nth-value 3 (parse-ordinary-lambda-list lambda-list))))

(defun lambda-list-vars (lambda-list)
  (multiple-value-bind (req opt rest key aok aux)
      (parse-ordinary-lambda-list lambda-list)
    (declare (ignore aok))
    (remove nil
            (append req
                    (mapcar #'car opt)
                    (list rest)
                    (mapcar #'cadar key)
                    (mapcar #'car aux)))))

(defmacro deftag (name (body attrs-var &rest ll) &body tag)
  (multiple-value-bind (tag decls docstring)
      (parse-body tag :documentation t)
    (with-gensyms (tmp-body)
      `(defmacro ,name (&body ,tmp-body)
         ,@(and docstring (list docstring))
         (multiple-value-bind  (,tmp-body ,attrs-var)
             (parse-deftag-body ,tmp-body)
           (destructuring-bind ,(if (symbolp body) `(&rest ,body) body)
               ,tmp-body
             ,@decls
             ;; Bind the keywords to the provided arguments.
             (destructuring-bind ,(allow-other-keys ll)
                 ,attrs-var
               ;; Remove the keywords from the attributes.
               (let ((,attrs-var (remove-from-plist ,attrs-var ,@(extract-lambda-list-keywords ll))))
                 (list 'with-html ,@tag)))))))))
