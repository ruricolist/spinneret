(in-package #:spinneret)

(in-nomine:define-namespace deftag
  :name-type symbol
  :value-type (function (list) list))

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

(defmacro deftag/keyword (name (body attrs-var &rest ll) &body tag)
  "Base case for a deftag that does not define a macro."
  (when (eql attrs-var '&key)
    (error "Missing attributes variable."))
  (mvlet* ((tag decls docstring
            (parse-body tag :documentation t))
           ;; Remove the keywords from the attributes.
           (attrs
            `(remove-from-plist ,attrs-var ,@(extract-lambda-list-keywords ll))))
    (with-gensyms (tmp-body)
      `(progn
         (eval-always
           (setf (symbol-deftag ',name)
                 (lambda (,tmp-body)
                   ,docstring
                   (multiple-value-bind  (,tmp-body ,attrs-var)
                       (parse-deftag-body ,tmp-body)
                     (destructuring-bind ,(if (symbolp body) `(&rest ,body) body)
                         ,tmp-body
                       ,@decls
                       ;; Bind the keywords to the provided arguments.
                       (destructuring-bind ,(allow-other-keys ll)
                           ,attrs-var
                         (let ((,attrs-var ,attrs))
                           (list 'with-html ,@tag))))))))
         ',name))))

(defmacro deftag/macro (name (body attrs-var &rest ll) &body tag)
  "A deftag that also defined a macro."
  (mvlet* ((tag decls docstring
            (parse-body tag :documentation t)))
    (declare (ignore decls))
    `(progn
       (deftag/keyword ,name (,body ,attrs-var ,@ll) ,@tag)
       (defmacro ,name (&body ,body)
         ,@(and docstring (list docstring))
         (deftag-expand ',name ,body :error t)))))

(defmacro deftag (name (body attrs-var &rest ll) &body tag)
  "Define NAME as a tag.
If NAME is not a keyword, it will also be defined as a macro with an
implicit `with-html'."
  (let ((definer
          (if (keywordp name)
              'deftag/keyword
              'deftag/macro)))
    `(,definer ,name (,body ,attrs-var ,@ll) ,@tag)))

(defun deftag-expand (element args &key error)
  (cond ((deftag-boundp element)
         (funcall (symbol-deftag element) args))
        (error (symbol-deftag element))
        (t (cons element args))))
