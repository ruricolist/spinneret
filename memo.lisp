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

(defun weak-memo (f)
  "Memoize using a value-weak hash table."
  (let ((cache (tg:make-weak-hash-table :test 'equal :weakness :value)))
    (lambda (&rest args)
      (multiple-value-bind (value found?)
          (gethash args cache)
        (if found?
            value
            (setf (gethash args cache)
                  (apply f args)))))))

(defun memoize-weakly (f)
  (setf (symbol-function f)
        (weak-memo (symbol-function f))))
