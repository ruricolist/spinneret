;; We define this package to make ASDF package-inferred
;; systems happy
(defpackage #:spinneret/cl-markdown
  (:use #:cl))
(in-package #:spinneret/cl-markdown)

;; Here we redefine a function inside of the main
;; package, to change it's behavior
(defun spinneret::parse-as-markdown (string)
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
            (spinneret::trim-ends "<p>" expansion "</p>")))))
