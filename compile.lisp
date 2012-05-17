;;; Functions used only at compile time.

(in-package #:spinneret)

(defun parse-html (form env)
  (labels ((rec (form)
             (cond ((atom form) form)
                   ((constantp form env) form)
                   ((dotted-list? form) form)
                   ((eql (car form) 'with-tag) form)
                   ((keywordp (car form))
                    (let ((form (pseudotag-expand (car form) (cdr form))))
                      (if (keywordp (car form))
                          (multiple-value-bind (name attrs body)
                              (tag-parts form)
                            `(with-tag (,name ,@attrs)
                               ,@(mapcar #'rec body)))
                          form)))
                   ((stringp (car form))
                    (destructuring-bind (control-string . args)
                        form
                      (let ((cs (parse-as-markdown control-string)))
                        `(format-text
                          ,@(if (and args (every #'constantp args))
                                (list (apply #'format nil cs args))
                                `((formatter ,cs) ,@args))))))
                   (t (cons (rec (car form))
                            (mapcar #'rec (cdr form)))))))
    (rec form)))

(defun dotted-list? (list)
  (declare (cons list))
  (not (null (cdr (last list)))))

(defun tag-parts (form)
  "Divide a form into an element, attributes, and a body. Provided
the form qualifies as a tag, the element is the car, the attributes
are all the following key-value pairs, and the body is what remains."
  (when (keywordp (car form))
    (let ((tag (car form))
          (body (cdr form))
          attrs classes)
      (loop (if (keywordp (car body))
                (if (eql (car body) :class)
                    (progn
                      (push (nth 1 body) classes)
                      (setf body (cddr body)))
                    (setf attrs (nconc attrs
                                       ;; Rather than subseq, in case of
                                       ;; an empty attribute.
                                       (list (nth 0 body)
                                             (nth 1 body)))
                          body (cddr body)))
                (return
                  (values
                   tag
                   (nconc
                    (when classes
                      `(:class
                        ,(if (every #'stringp classes)
                             (apply #'class-union (nreverse classes))
                             `(class-union ,@(nreverse classes)))))
                    attrs)
                   body)))))))

(defun class-union (&rest classes)
  (with-output-to-string (s)
    (let ((classes (remove-duplicates classes :test #'equal)))
      (when classes
        (write-string (car classes) s)
        (when (cdr classes)
          (dolist (c (cdr classes))
            (write-char #\Space s)
            (write-string c s)))))))

(defmacro with-tag ((name &rest attributes) &body body)
  (let ((empty? (not body))
        (pre? (not (null (preformatted? name)))))
    `(prog1 nil
       (let ((*depth* (+ *depth* 1)))
         ,@(start-tag-forms name attributes empty?)
         (without-trailing-space
           ,@(loop for form in body
                   collect `(catch-string ,form ,pre?)))
         ,@(unless empty?
             (end-tag-forms name))))))

(defun start-tag-forms (element attrs empty?)
  (let (forms)
    (when (invalid? element)
      (push `(note-invalid ,element) forms))
    (unless (inline? element)
      (push '(newline-and-indent) forms))
    (push `(write-string ,(format nil "<~(~A~)" element) *html*) forms)
    (if attrs
        (let ((attrs (escape-attrs attrs)))
          (push `(format-attributes ,@attrs) forms))
        (push '(write-char #\> *html*) forms))
    (when (and empty? (not (void? element)))
      (push `(write-string ,(format nil "</~(~A~)>" element) *html*) forms))
    (unless (or (inline? element)
                (paragraph? element))
      (push '(terpri *html*) forms))
    (nreverse forms)))

(defun end-tag-forms (element)
  (unless (or (void? element)
              (unmatched? element))
    (let (forms)
      (unless (or (inline? element)
                  (paragraph? element))
        (push '(fresh-line *html*) forms))
      (push `(justify-end-tag ,(format nil "</~(~A~)>" element)) forms)
      (nreverse forms))))

(defun escape-attrs (attrs)
  (loop for (attr val . rest) on attrs by #'cddr
        if (eql attr :dataset)
          append (escape-attrs
                  (loop for (attr val . rest) on attrs by #'cddr
                        collect (make-keyword "data-" attr)
                        collect val))
        else if (or (stringp val)
                    (numberp val)
                    (characterp val))
               collect attr and collect (escape-value val)
        else
          collect attr and collect `(escape-value ,val)))

(defun parse-as-markdown (string)
  "Expand STRING as markdown only if it contains markdown."
  (declare (string string))
  (let ((expansion
          (with-output-to-string (s)
            (let (markdown:*parse-active-functions*
                  markdown:*render-active-functions*)
              (markdown:markdown string
                                 :stream s
                                 :format :html)))))
    (if (search string expansion)
        string
        (if (find #\Newline string)
            expansion
            (trim-ends "<p>" expansion "</p>")))))

(defun trim-ends (prefix string suffix)
  (declare (string prefix string suffix))
  (let ((pre (mismatch string prefix))
        (suf (mismatch string suffix :from-end t)))
    (subseq string
            (if (= pre (length prefix)) pre 0)
            (if (= suf (- (length string) (length suffix)))
                suf
                (length string)))))
