(in-package #:spinneret)

(export '(deftemplate))

(defmacro deftemplate (name args &body body &environment env)
  (if (null args)
      `(defun ,name () (with-html ,@body))
      (multiple-value-bind (new-arglist gensym-pairs)
          (escape-arglist args env)
        (let ((vars (mapcar #'car gensym-pairs)))
          (with-gensyms (whole)
            `(eval-and-compile
               (setf (get ',name 'template)
                     (compile nil
                              (lambda ,new-arglist
                                (symbol-macrolet ,(loop for (v . g) in gensym-pairs
                                                        collect `(,v (eval* ,g)))
                                  (with-html ,@body)))))
               (defmacro ,name (&whole ,whole ,@args)
                 (declare (ignore ,@vars))
                 `(call-template ',',name ,@(cdr ,whole)))))))))

(defmacro call-template (name &rest args)
  (flet ((string-gensym (x)
           (gensym (string x)))
         (could-be-keyword? (x)
           (constantp x)))
    (let* ((params (remove-if #'could-be-keyword? args))
           (cells
             (loop for param in params
                   unless (could-be-keyword? param)
                     collect (string-gensym 'cell)))
           (guards
             (loop for nil in params
                   collect (string-gensym 'guard)))
           (thunks
             (loop for nil in params
                   collect (string-gensym 'thunk)))
           (fns
             (loop for thunk in thunks
                   collect `(function ,thunk))))
      `(let (,@cells ,@guards)
         (flet ,(loop for param in params
                      for cell in cells
                      for guard in guards
                      for thunk in thunks
                      collect `(,thunk
                                ()
                                (if ,guard
                                    ,cell
                                    (setf ,guard t
                                          ,cell (with-html ,param)))))
           (declare (dynamic-extent ,@fns))
           (funcall
            (get ,name 'template)
            ,@(loop for arg in args
                    if (could-be-keyword? arg)
                      collect arg
                    else collect (pop fns))))))))

(defvar *quote* nil)

(defun eval* (x)
  (if *quote*
      x
      (typecase x
        (list (mapc #'funcall x))
        (function (funcall x))
        (t x))))

(defmacro do-elements ((var arg) &body body)
  (with-gensyms (temp)
    `(dolist (,temp (let ((*quote* t))
                      ,arg))
       (symbol-macrolet ((,var (eval* ,temp)))
         ,@body))))

(defun escape-arglist (arglist env)
  ;; Returns two values: the escaped arglist and an alist of variable
  ;; names and gensyms.
  (multiple-value-bind (required optional rest
                        keywords allow-other-keys aux)
      (alexandria:parse-ordinary-lambda-list (substitute '&rest '&body arglist))
    (let ((gensym-pairs '()))
      ;; TODO Rewrite the initforms?
      (flet ((string-gensym (arg)
               (gensym (string arg)))
             (unsplice (x)
               (if x (list x) nil))
             (check-initform (initform)
               (unless (constantp initform env)
                 (error "Initforms must be constant."))))
        (values
         (append (loop for arg in required
                       for g = (string-gensym arg)
                       do (push (cons arg g) gensym-pairs)
                       collect g)
                 (when optional
                   (cons '&optional
                         (loop for (name init suppliedp) in optional
                               for g = (string-gensym name)
                               do (check-initform init)
                                  (push (cons name g) gensym-pairs)
                               collect `(,g ,init ,@(unsplice suppliedp)))))
                 (when rest
                   (let ((g (string-gensym rest)))
                     (push (cons rest g) gensym-pairs)
                     (list '&rest g)))
                 (when keywords
                   (cons '&key
                         (loop for ((kw-name name) init supplied) in keywords
                               for g = (string-gensym name)
                               do (check-initform init)
                                  (push (cons name g) gensym-pairs)
                               collect `((,kw-name ,g) ,init))))
                 (when allow-other-keys
                   (list '&allow-other-keys))
                 (when aux (cons '&aux (sublis gensym-pairs aux))))
         (nreverse gensym-pairs))))))

(defmacro eval-and-compile (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))
