(in-package :spinneret)

(defun interpret-html-tree (tree &key
                                   ((:stream *html*) *html*)
                                   ((:style *html-style*) :tree))
  "Interpet TREE as HTML.
The syntax used is roughly that of Spinneret.
"
  (match tree
    ((list* (and name (type keyword)) attrs-and-body)
     (multiple-value-bind (attrs body)
         (parse-leading-keywords attrs-and-body)
       (dynamic-tag :name name :attrs attrs
         (mapc #'interpret-html-tree body)
         nil)))
    (otherwise
     (html tree))))
