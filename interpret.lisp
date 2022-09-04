(in-package :spinneret)

(defun interpret-html-tree (tree &key
                                   ((:stream *html*) *html*)
                                   ((:style *html-style*) :tree)
                                   ((expanded expanded) nil))
  "Interpet TREE as HTML.
The syntax used is roughly that of Spinneret.
"
  (match tree
    ((list* (and _ (eql :tag)) attrs-and-body)
     (multiple-value-bind (attrs body)
         (parse-leading-keywords attrs-and-body)
       (let ((name (getf attrs :name)))
         (unless name
           (error "No name for dynamic tag: ~a" tree))
         (interpret-html-tree
          `(,name ,@(remove-from-plist attrs :name)
                  ,@body)))))
    ((list* (and tag (type keyword)) attrs-and-body)
     (if-let (expander
              (and (not expanded)
                   (pseudotag-expander tag)))
       (interpret-html-tree
        (let ((*interpret* t))
          (apply expander attrs-and-body))
        'expanded t)
       (multiple-value-bind (attrs body)
           (parse-leading-keywords attrs-and-body)
         (dynamic-tag :name tag :attrs attrs
           (mapc #'interpret-html-tree body)
           nil))))
    (otherwise
     (html tree))))
