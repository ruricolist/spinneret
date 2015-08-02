(in-package #:spinneret)

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
