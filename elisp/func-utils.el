; -*- lexical-binding: t -*-

(require 'cl-lib)


(defun const (constant)
  (lambda (&rest ignored)
    constant))

(defun id (x)
  x)

(defun partial (proc &rest args0)
  (lambda (&rest args1)
    (apply proc (append args0 args1))))

(defun compose (&rest procs)
  (setf procs (reverse procs))
  (lambda (&rest args)
    (let ((my-procs procs))
      (while my-procs
        (setf args (list (apply (car my-procs) args))
              my-procs (cdr my-procs)))
      (car args))))

(defmacro mcompose (expr)
  (let* ((max-used-arg 0)
         ;; Expand expression, keeping track of the highest-numbered argument used
         ;; Leaving arguments and function application as macros to be expanded
         ;; As we don't know how many arguments are accessed explicitly yet
         (composed-expr (cl-labels ((aux (expr)
                                         (cond ((and (consp expr) (equal 'lisp (car expr)))
                                                `(progn ,@(cdr expr)))
                                               ((and (consp expr) (numberp (car expr)))
                                                (setf max-used-arg (max max-used-arg (car expr)))
                                                `(expand-arg ,(car expr)))
                                               ((and (consp expr) (equal 'quote (car expr)))
                                                expr)
                                               ((functionp expr)
                                                `(expand-apply ,expr))
                                               ((consp expr)
                                                (cons (car expr)
                                                      (mapcar #'aux (cdr expr))))
                                               (t
                                                expr))))
                          (aux expr)))
         (params (mapcar (lambda (n)
                           (gensym (format-message "mcompose-param-%d-" n)))
                         (number-sequence 1 max-used-arg)))
         (rest-param (gensym "mcompose-rest-param")))
    ;; Substitute in arguments and function applications through macros
    `(cl-macrolet ((expand-arg (n) (nth (- n 1) ',params))
                   (expand-apply (fun) `(apply #',fun ,@',params ,',rest-param)))
       (lambda (,@params &rest ,rest-param)
         ,composed-expr))))

(defun gurd-map (f first &rest rest)
  (apply 'cl-map (type-of first) f first rest))


(defun arglist->args (arglist &optional always-rest-arg)
  (let ((args nil)
        (rest-arg-found nil))
    (while arglist
      (let ((first (pop arglist)))
        (cond ((eq first '&optional) t)
              ((eq first '&rest)
               (push (pop arglist) args)
               (setf rest-arg-found t)
               (cl-assert (not arglist)))
              (t (push first args)))))
    (when (and always-rest-arg (not rest-arg-found))
      (push nil args))
    (reverse args)))


(defun recur-call (fun &rest args)
  (let ((fun+args (cons fun args)))
    (catch 'recur-return
      (while t
        (let ((fun (car fun+args))
              (args (cdr fun+args)))
          (setf fun+args
                (catch 'recur
                  (throw 'recur-return
                         (if (recurp fun)
                             (apply (get fun 'recur) args)
                           (apply fun args))))))))))

(cl-defmacro with-recur (&body body)
  `(cl-macrolet ((recur (&body expr)
                        `(throw 'recur (list ,@expr))))
     ,@body))

(defun recurp (fun)
  (and (symbolp fun) (get fun 'recur)))

(defun recur-get-key (fun)
  (and (symbolp fun) (get fun 'recur)))

(cl-defmacro recur-lambda (arglist &body body)
  (let ((lambda-symbol (gensym "lambda-symbol")))
    `(progn
       (put ',lambda-symbol 'recur (lambda ,arglist (with-recur ,@body)))
       (fset ',lambda-symbol (lambda ,arglist (apply #'recur-call
                                                     ',lambda-symbol
                                                     ,@(arglist->args arglist t))))
       ',lambda-symbol)))

(cl-defmacro defrecur (name arglist &optional docstring &body body)
  (unless (stringp docstring)
    (setf body (cons docstring body)
          docstring nil))
  `(progn (defun ,name ,arglist
            ,@(when docstring (list docstring))
            (apply #'recur-call
                   ',name
                   ,@(arglist->args arglist t)))
          (put ',name 'recur (lambda ,arglist (with-recur ,@body)))
          ',name))

(provide 'func-utils)
