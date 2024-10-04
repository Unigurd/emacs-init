; -*- lexical-binding: t -*-

(require 'generator)
(require 'func-utils)

;; iter-entangle

(define-error 'iter-defun2-invalid-initial-input "Invalid form for INITIAL-INPUT." 'error)

(eval-and-compile
  (defun iter--lambda2-internals (arglist initial-input body)
    (let ((iter-var (gensym "iter")))
      `(let ((,iter-var (funcall (iter-lambda ,arglist
                                   ,@(pcase initial-input
                                       (`nil body)
                                       (`(,arg)
                                        (list `(let ((,arg (iter-yield nil)))
                                                 ,@body)))
                                       (_ (signal 'iter-defun2-invalid-initial-input initial-input))))
                                 ,@(arglist->args arglist))))
         (iter-next ,iter-var)
         ,iter-var)))

  (cl-defmacro iter-lambda2 (arglist initial-input &body body)
    "Like ITER-LAMBDA, but the first value passed with ITER-NEXT does matter.
Written like (iter-lambda2 (a b) (c) ...) where
A and B are passed to the generator and C is the first value passed with
ITER-NEXT."
    (declare (indent defun))
    `(lambda ,arglist
       ,(iter--lambda2-internals arglist initial-input body)))

  (cl-defmacro iter-defun2 (name arglist initial-input docstring-or-body &body body)
    "Like ITER-DEFUN, but the first value passed with ITER-NEXT does matter.
Written like (iter-defun2 my-iter (a b) (c) \"docstring\" ...) where
A and B are passed to the generator and C is the first value passed with
ITER-NEXT."
    (declare (indent defun))
    (cl-destructuring-bind (docstring body) (if (stringp docstring-or-body)
                                                (list (list docstring-or-body) body)
                                              (list nil (cons docstring-or-body body)))
      `(defun ,name ,arglist
         ,@docstring
         ,(iter--lambda2-internals arglist initial-input body)))))

(defmacro iter-yield-from2 (iter &optional arg)
  "When used inside a generator function, delegate to a sub-iterator.
The values that the sub-iterator yields are passed directly to
the caller, and values supplied to `iter-next' are sent to the
sub-iterator.  `iter-yield-from' evaluates to the value that the
sub-iterator function returns via `iter-end-of-sequence'."
  (let ((errsym (cps--gensym "yield-from-result"))
        (itersym (cps--gensym "yield-from-iter")))
    `(let ((,itersym ,iter))
       (unwind-protect
           (condition-case ,errsym
               (let ((vs ,arg))
                 (while t
                   (setf vs (iter-yield (iter-next ,itersym vs)))))
             (iter-end-of-sequence (cdr ,errsym)))
         (iter-close ,itersym)))))

(iter-defun iter (&rest args)
  (dolist (arg args)
    (iter-yield arg)))

(defun iter-retval (iter)
  (condition-case cond
      (while t
        (iter-next iter))
    (iter-end-of-sequence (cdr cond))))

(iter-defun iter-propagate (iter &optional acc fun)
  (unless fun (setf fun (lambda (_ x) x)))
  (while t
    (let ((next (iter-next iter acc)))
      (setf acc (funcall fun acc next))
      (iter-yield next))))

(iter-defun iter-prefix (f acc iter)
  (iter-do (elm iter)
    (setf acc (funcall f acc elm))
    (iter-yield acc)))

;; (defun iter-prefix-sum (iter)
;;   (iter-propagate (iter-supply (iter-yield-prev 0) iter) 0 '+))

;; (iter->list (iter-prefix-sum (iter 1 2 3 4 5 6 7)))

(iter-defun empty-iter (&optional retval) retval)

(iter-defun list->iter (list)
  (dolist (elm list)
    (iter-yield elm)))

(defun iter->list (iter)
  (let ((list nil))
    (iter-do (var iter)
      (setf list (cons var list)))
    (reverse list)))

(defun iter->list* (iter)
  (let* ((list (list 'tmp))
         (current list)
         (continuep t))
    (while continuep
      (condition-case x
          (setf (cdr current) (list (iter-next iter))
                current (cdr current))
        (iter-end-of-sequence
         (setf (cdr current) (cdr x)
               continuep nil))))
    (cdr list)))

(defun iter-const (&rest xs)
  (list->iter xs))

(iter-defun iter-cons (elm iter)
  (iter-yield elm)
  (iter-yield-from iter))

(iter-defun iter-snoc (iter elm)
  (iter-yield-from iter)
  (iter-yield elm))

(iter-defun iter-rest (iter)
  (iter-next iter)
  (iter-yield-from iter))

(iter-defun iter-map (proc iter)
  (iter-do (value iter)
    (iter-yield (funcall proc value))))

(iter-defun iter-append (&rest iters)
  (while iters
    (iter-yield-from (car iters))
    (pop iters)))

(iter-defun iter-merge (cmp &rest iters)
  (unless cmp (setf cmp #'<=))
  (let* ((next-value (lambda (iter)
                       (condition-case nil
                           (cons (iter-next iter) iter)
                         (iter-end-of-sequence (cons nil nil)))))
         (remove-finished-iters (partial #'cl-remove-if-not #'cdr))
         (pairs (funcall remove-finished-iters (mapcar next-value iters))))
    (while pairs
      (let ((chosen (car pairs))
            (rest (cdr pairs)))
        (while rest
          (when (funcall cmp (caar rest) (car chosen))
            (setf chosen (car rest)))
          (setf rest (cdr rest)))
        (iter-yield (car chosen))
        (let ((next-from-chosen-iter (funcall next-value (cdr chosen))))
          (setf (car chosen) (car next-from-chosen-iter)
                (cdr chosen) (cdr next-from-chosen-iter)
                pairs (funcall remove-finished-iters pairs)))))))


(iter-defun iter-unique (iter &optional eq)
  (unless eq (setf eq #'equal))
  (let ((last (iter-next iter)))
    (iter-yield last)
    (iter-do (next iter)
      (unless (funcall eq last next)
        (setf last next)
        (iter-yield last)))))

(iter-defun iter-filter (pred iter)
  (iter-do (elm iter)
    (when (funcall pred elm)
      (iter-yield elm))))

(iter-defun iter-loop (iter)
  "Note: stores all elements of iter in memory."
  (let ((list nil))
    (iter-do (elm iter)
      (push elm list)
      (iter-yield elm))
    (setf list (reverse list))
    (while list
      (iter-yield-from (list->iter list)))))

(iter-defun2 iter-take (n iter) (arg)
  (while (> n 0)
    (setf arg (iter-yield (iter-next iter arg)))
    (cl-decf n)))

(iter-defun iter-take-while (pred iter)
  (let ((elm nil))
    (while (funcall pred (setf elm (iter-next iter)))
      (iter-yield elm))))

(iter-defun2 iter-drop (n iter) (val)
  (if (listp n)
      (cl-loop for x in n do (iter-next iter x))
    (cl-loop repeat n do (iter-next iter)))
  (iter-yield-from2 iter val))

(iter-defun iter-drop-last (iter)
  "Drop last element of ITER."
  (let ((current (iter-next iter))
        (next (iter-next iter)))
    (while t
      (iter-yield current)
      (setf current next
            next (iter-next iter)))))


(iter-defun iter-zip (&rest iters)
  (while t
    (iter-yield (mapcar 'iter-next iters))))

(iter-defun iter-zip-seq (iters)
  (while t
    (iter-yield (gurd-map 'iter-next iters))))

(defun iter-next-default (iter &optional default arg)
  (condition-case _
      (iter-next iter arg)
    (iter-end-of-sequence default)))

(iter-defun iter-supply (iter args)
  (iter-yield (iter-next iter))
  (iter-do (arg args)
    (iter-yield (iter-next iter arg)))
  (iter-yield-from iter))

(iter-defun iter-yield-prev (&optional init)
  (let ((arg init))
    (while t
      (setf arg (iter-yield arg)))))

(iter-defun iter-count (&optional beg end)
  (unless beg (setf beg 0))
  (while (or (not end) (< beg end))
    (iter-yield beg)
    (cl-incf beg)))

(provide 'iter-utils)


