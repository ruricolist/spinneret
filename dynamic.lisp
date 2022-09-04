(in-package :spinneret)

(defun expand-dynamic-tag (&rest args)
  `(dynamic-tag ,@args))

(deftag dynamic-tag (body attrs &key name)
  (unless name
    (error "No tag name"))
  (let ((empty? (null body))
        (thunk (gensym (string 'dynamic-tag-thunk)))
        (attrs (escape-attrs nil attrs)))
    `(prog1 nil
       (flet ((,thunk ()
                ,@(loop for expr in body
                        collect `(catch-output ,expr))))
         (declare (dynamic-extent #',thunk))
         (dynamic-tag* ,name
                       (list ,@attrs)
                       #',thunk
                       ,empty?)))))

(defun expand-h* (&rest args)
  (if *interpret*
      (cons (heading-depth-heading)
            args)
      `(h* ,@args)))

(deftag h* (body attrs &key)
  `(dynamic-tag
     :name (heading-depth-heading)
     ,@attrs
     ,@body))
