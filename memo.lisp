(in-package #:spinneret)

(defun memo (f)
  (let ((cache (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (multiple-value-bind (value found?)
          (gethash args cache)
        (if found?
            value
            (setf (gethash args cache)
                  (apply f args)))))))

(defun memoize (f)
  (setf (symbol-function f)
        (memo (symbol-function f))))
