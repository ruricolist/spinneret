(in-package #:spinneret)

(export '(deftemplate))

(defmacro deftemplate (name args &body body)
  (if (null args)
      `(defun ,name () (with-html ,@body))
      (multiple-value-bind (new-arglist gensym-pairs)
          (escape-arglist args)
        (let ((vars (mapcar #'car gensym-pairs)))
          (with-gensyms (whole)
            `(eval-and-compile
               (setf (get ',name 'template)
                     (compile nil
                              (lambda ,new-arglist
                                (symbol-macrolet ,(loop for (v . g) in gensym-pairs
                                                        collect `(,v (call ,g)))
                                  (with-html ,@body)))))
               (defmacro ,name (&whole ,whole ,@args)
                 (declare (ignore ,@vars))
                 `(call-template ',',name ,@(cdr ,whole)))))))))

(defmacro call-template (name &rest args &environment env)
  `(funcall
    (get ,name 'template)
    ,@(loop for arg in args
            if (constantp arg env)
              collect arg
            else collect `(delay (with-html ,arg)))))

(defmacro delay (exp)
  (let ((value (gensym)))
    `(let (,value)
       (lambda ()
         (or ,value (setf ,value ,exp))))))

(defun force (thunk)
  (if (functionp thunk)
      (funcall thunk)
      thunk))

(defvar *quote* nil)

(defun call (x)
  (if *quote*
      x
      (typecase x
        (list (mapc #'force x))
        (function (force x))
        (t x))))

(defmacro do-elements ((var arg) &body body)
  (with-gensyms (temp)
    `(dolist (,temp (let ((*quote* t)) ,arg))
       (symbol-macrolet ((,var (call ,temp)))
         ,@body))))

(defun escape-arglist (arglist)
  ;; Returns two values: the escaped arglist and an alist of variable
  ;; names and gensyms.
  (multiple-value-bind (required optional rest
                        keywords allow-other-keys aux)
      (alexandria:parse-ordinary-lambda-list (substitute '&rest '&body arglist))
    (let ((gensym-pairs '()))
      (labels ((mkgensym (arg)
                 (or (cdr (rassoc arg gensym-pairs :test #'string=))
                     (let ((g (gensym (string arg))))
                       (prog1 g
                         (setf gensym-pairs (cons (cons arg g) gensym-pairs)))))))
        (values
         (append (loop for arg in required
                       collect (mkgensym arg))
                 (when optional
                   (cons '&optional
                         (loop for (name init suppliedp) in optional
                               collect `(,(mkgensym name) ,init
                                         ,@(when suppliedp (list suppliedp))))))
                 (when rest
                   (list '&rest (mkgensym rest)))
                 (when keywords
                   (cons '&key
                         (loop for ((kw-name name) init supplied) in keywords
                               collect `((,kw-name ,(mkgensym name)) ,init))))
                 (when allow-other-keys
                   (list '&allow-other-keys))
                 (when aux (cons '&aux (sublis gensym-pairs aux))))
         (nreverse gensym-pairs))))))

(defmacro eval-and-compile (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))
