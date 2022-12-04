(in-package :spinneret)

(defun interpret-html-tree (tree &key
                                   (stream *html*)
                                   ((:style *html-style*) :tree))
  "Interpet TREE as HTML.
The syntax used is roughly that of Spinneret.
"
  (let ((*html* stream))
    (labels ((interpret-html-tree (tree &optional expanded)
               (match tree
                 ;; Handle the (:tag :name "mytag") syntax for dynamic tags.
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
                    ;; Handle interpreting a pseudotag.
                    (interpret-html-tree
                     (let ((*interpret* t))
                       (apply expander attrs-and-body))
                     t)
                    (receive (tag attrs body)
                        (tag-parts tree)
                      (dynamic-tag :name tag :attrs attrs
                        (mapc #'interpret-html-tree body)
                        nil))))
                 (otherwise
                  (html tree)))))
      (interpret-html-tree tree))))
