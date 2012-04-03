(in-package #:spinneret)

(memoize
 (defun in? (element var)
   (not (null (find (string element)
                    (symbol-value var)
                    :key #'string
                    :test #'string=)))))

(defparameter *void-elements*
  '(!doctype area base br col command embed hr img
    input keygen link meta param source track wbr))

(defun void? (element)
  (in? element '*void-elements*))

(defparameter *literal-elements*
  '(pre script style))

(defun literal? (element)
  (in? element '*literal-elements*))

(defparameter *inline-elements*
  '(a abbr address bdo small code samp kbd
    cite strong dfn br em q data time var
    sub sup i b s u mark ruby rt rp bdi span wbr
    ins del col meter output))

(defun inline? (element)
  ;; TODO
  (values (in? element '*inline-elements*)))

(defparameter *paragraph-elements*
  '(meta title button label li h1 h2 h3 h4 h5 h6 p legend option
    dt dd figcaption iframe colgroup td th output summary command))

(defun paragraph? (element)
  (in? element '*paragraph-elements*))

(defparameter *end-tag-optional*
  ;; html head body
  '(li dt dd p rt rp optgroup
    option colgroup thead tbody tfoot tr td th))

(defun unmatched? (element)
  (in? element '*end-tag-optional*))

(defparameter *preformatted*
  '(pre textarea script style))

(defun preformatted? (element)
  (in? element '*preformatted*))

(defparameter *pseudotags*
  '((doctype . make-doctype)
    (!doctype . make-doctype)
    (cdata . make-cdata)
    (!-- . make-comment)
    (comment . make-comment)
    (html . make-html)
    (head . make-head)))

(memoize
 (defun pseudotag-expander (element)
   (cdr (assoc (string element) *pseudotags*
               :test #'string= :key #'string))))

(defun pseudotag-expand (element args)
  (let ((expander (pseudotag-expander element)))
    (if expander
        (apply expander args)
        (cons element args))))

(defparameter *html5-elements*
  '(a abbr address area article aside audio b base bdi bdo blockquote
    body br button canvas caption cite code col colgroup command data
    datalist dd del details dfn div dl dt em embed fieldset
    figcaption figure footer form head h1 h2 h3 h4 h5 h6 header
    hgroup hr html i iframe img input ins kbd keygen label legend li
    link map mark menu meta meter nav noscript object ol
    optgroup option output p param pre progress q rp rt ruby s samp
    script section select small source span strong style sub summary
    sup table tbody td textarea tfoot th thead time title tr
    track u ul var video wbr))

(defun valid? (element)
  (in? element '*html5-elements*))

(defun invalid? (element)
  (not (valid? element)))

(defparameter *embedded-content*
  '(math svg))

(defun embedded? (element)
  (in? element '*embedded-content*))
