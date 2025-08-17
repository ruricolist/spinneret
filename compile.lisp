;;; Functions used only at compile time.

(in-package #:spinneret)

(defun live-deftag-form? (form)
  "If FORM starts with a deftag, return non-nil."
  (and (symbolp (car form))
       (deftag-boundp (car form))))

(defun parse-html (form env)
  (labels ((rec (form)
             (cond
               ;; There's nothing we can do with an atom.
               ((atom form) form)
               ;; There's nothing we can do with an improper list, either.
               ((dotted-list? form) form)
               ;; If the form is constant, leave it to be inlined.
               ((ignore-errors (constantp form env)) form)
               ;; Don't descend into nested with-tag forms.
               ((eql (car form) 'with-tag) form)
               ((eql (car form) :disable-html)
                (if (null (cddr form))
                    (cadr form)
                    `(progn ,@(cdr form))))
               ;; Compile as a tag.
               ((keywordp (car form))
                (mvlet* ((name attrs body (tag-parts form))
                         ;; Canonical form, without inline ids or tags.
                         (form (append (list name) attrs body))
                         (form (pseudotag-expand (car form) (cdr form)))
                         (form (deftag-expand (car form) (cdr form))))
                  (if (not (keywordp (car form))) form
                      (mvlet* ((name attrs body (tag-parts form)))
                        (if (live-deftag-form? form) form
                            (if (valid? name)
                                (let ((body (mapcar #'rec body)))
                                  (if (valid-custom-element-name? name)
                                      `(with-custom-element (,name ,@attrs)
                                         ,@body)
                                      `(with-tag (,name ,@attrs)
                                         ,@body)))
                                (cons (car form)
                                      (mapcar #'rec (cdr form)))))))))
               ;; Don't descend into non-keyword deftags.
               ((live-deftag-form? form) form)
               ;; Compile as a format string (possibly using Markdown).
               ((stringp (car form))
                (destructuring-bind (control-string . args)
                    form
                  (let ((cs (parse-as-markdown control-string)))
                    `(format-text
                      ,@(if (and args (every (lambda (arg) (constantp arg env)) args))
                            (list (format nil "~?" cs
                                          (mapcar #'escape-to-string args)))
                            `((formatter ,cs)
                              ,@(loop for arg in args
                                      ;; Escape literal strings at
                                      ;; compile time.
                                      if (typep arg 'string env)
                                        collect (escape-to-string arg)
                                      else collect `(xss-escape ,arg))))))))
               ;; Keep going.
               (t (cons (rec (car form))
                        (mapcar #'rec (cdr form)))))))
    (rec form)))

(defun dotted-list? (list)
  (declare (cons list))
  (not (null (cdr (last list)))))

(defun dissect-tag (tag)
  "Dissect a tag like `:div.class#id' into the tag itself and a plist
of attributes."
  (if (notany (lambda (c)
                (member c '(#\# #\.)))
              (string tag))
      (values tag nil)
      (destructuring-bind (tag . parts)
          (split "([.#])" (string-downcase tag) :with-registers-p t)
        (values (make-keyword (string-upcase tag))
                (sublis '(("." . :class)
                          ("#" . :id))
                        parts
                        :test #'equal)))))

(defun simplify-tokenized-attributes (attrs)
  "Return an alist of the tokenized attributes (like :class) and a
plist of the regular attributes."
  (let ((tokenized ()))
    (loop for (k v . nil) on attrs by #'cddr
          if (tokenized-attribute? k)
            do (push v (assoc-value tokenized k))
          else append (list k v) into regular
          finally (return
                    (append (tokenized-attributes-plist tokenized)
                            regular)))))

(defun tokenized-attributes-plist (alist)
  "When possible, join tokenized attributes at compile time."
  (loop for (tag . tokens) in alist
        append (let ((tokens (reverse tokens)))
                 `(,tag
                   ,(if (every (disjoin #'stringp #'null #'keywordp) tokens)
                        (apply #'join-tokens tokens)
                        `(join-tokens ,@tokens))))))

(defun join-tokens (&rest tokens)
  (when-let (tokens (remove-duplicates (remove nil tokens) :test #'equal))
    (with-output-to-string (s)
      (loop for (token . rest) on tokens do
        (eif (keywordp token)
             (format s "~(~a~)" token)
             (format s "~a" token))
        (when rest (write-char #\Space s))))))

(define-compiler-macro join-tokens (&whole call &rest tokens)
  (cond ((null tokens) nil)
        ((null (rest tokens))
         (let ((token (car tokens)))
           (once-only (token)
             `(and ,token (princ-to-string ,token)))))
        (t call)))

(defun tag-parts (form)
  "Parse a form into an element, attributes, and a body. Provided
the form qualifies as a tag, the element is the car, the attributes
are all the following key-value pairs, and the body is what remains."
  (when (keywordp (car form))
    (mvlet* ((tag-name body (car+cdr form))
             (tag tag-attrs (dissect-tag tag-name))
             (attrs body
              (parse-leading-keywords (append tag-attrs body))))
      (values tag (simplify-tokenized-attributes attrs) body))))

(defun tag-thunk-name (name attrs)
  "Produce a helpful name for a thunk from NAME and ATTRS."
  (let ((id (getf attrs :id)))
    (fmt "<~a~@[#~a~]>" name id)))

(defun wrap-body-as-stack-thunk (thunk-name body form)
  `(prog1 nil
     (flet ((,thunk-name ()
              ,@(loop for expr in body
                      collect `(catch-output ,expr))))
       (declare (dynamic-extent (function ,thunk-name)))
       ,form)))

(defmacro with-tag ((name &rest attributes) &body body)
  (let* ((empty? (not body))
         (pre? (not (null (preformatted? name))))
         (tag-fn (or (tag-fn name) (error 'no-such-tag :name name)))
         (thunk (gensym (tag-thunk-name name attributes))))
    (wrap-body-as-stack-thunk
     thunk body
     `(,tag-fn
       (macrolet ((:raw (s)
                    `(escaped-string ,s)))
         (list ,@(escape-attrs name attributes)))
       #',thunk
       ,pre?
       ,empty?))))

(defmacro with-custom-element ((name &rest attrs) &body body)
  (check-type name keyword)
  (let* ((open (tag-open name))
         (close (tag-close name))
         (thunk (gensym (tag-thunk-name name attrs)))
         (empty? (null body)))
    (wrap-body-as-stack-thunk
     thunk body
     `(custom-elt-fn ,open ,close
                     (list ,@(escape-attrs name attrs))
                     #',thunk
                     ,empty?))))

(defun escape-attrs (tag attrs)
  (let ((attrs
          (loop for (attr val . nil) on attrs by #'cddr
                if (eql attr :dataset)
                  append (escape-attrs
                          tag
                          (loop for (attr val . nil) on val by #'cddr
                                collect (make-keyword (fmt "~:@(data-~A~)" attr))
                                collect val))
                else if (eql attr :attrs)
                       collect attr and collect val
                else if (or (stringp val)
                            (numberp val)
                            (characterp val))
                       collect attr and collect (escape-value val)
                else
                  collect attr and collect `(escape-value ,val))))
    (loop for (attr nil . nil) on attrs by #'cddr
          unless (valid-attribute? tag attr)
            do (warn "~A is not a valid attribute for <~A>"
                     attr tag))
    attrs))

(declaim (notinline parse-as-markdown))
(defun parse-as-markdown (string)
  "Placeholder, load spinneret/cl-markdown system if you want to expand
  markdown."
  string)

(defun trim-ends (prefix string suffix)
  (declare (string prefix string suffix))
  (let ((pre (mismatch string prefix))
        (suf (mismatch string suffix :from-end t)))
    (subseq string
            (if (= pre (length prefix)) pre 0)
            (if (= suf (- (length string) (length suffix)))
                suf
                (length string)))))
