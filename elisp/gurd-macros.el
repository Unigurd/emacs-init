;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)


(eval-and-compile
  (defun plet--expand (bindings body)
    (if (not bindings)
        (cons 'progn body)
      (cl-destructuring-bind
          (symbol-bindings bindings) (-split-with (lambda (binding) (symbolp (car binding))) bindings)
        (if symbol-bindings
            ;; Binding symbols with `let*'
            `(let* ,symbol-bindings
               ,(plet--expand bindings body))
          ;; Binding a place with `setf'
          (cl-destructuring-bind
              (place expr) (car bindings)
            ;; Variable that saves old value of the place to restore
            (let ((save-var (gensym (format "saved-%s" (car place)))))
              `(let ((,save-var ,place))                         ; Save
                 (unwind-protect                                 ; Protect
                     (progn (setf ,place ,expr)                  ; Bind
                            ,(plet--expand (cdr bindings) body)) ; Body
                   (setf ,place ,save-var)                       ; Restore
                   )))))))))


(cl-defmacro plet (bindings &body body)
  "`let*' that can bind to places and not just variables.
Always binds plain variables lexically."
  (declare (indent 1))
  (plet--expand bindings body))


(cl-defmacro unless-recursive (&body body)
  "Will evalute `body' unless this form is called recursively.
That is, (unless-recursive (unless-recursive (message \"hej\"))) will
  print \"hej\", but calling a function defined as
(defun foo () (unless-recursive (message \"hej\") (foo))) will only
  print \"hej\" once."
  (declare (indent 0))
  (let ((guard (gensym "guard-var")))
    `(progn
       (defvar ,guard nil)
       (unless (symbol-value ',guard)
         (plet (((symbol-value ',guard) t))
           ,@body)))))


(cl-defmacro unless-buffer-recursive (&body body)
  "Like `unless-recursive' but works per buffer."
  (declare (indent 0))
  (let ((guard (gensym "buffer-guard-var")))
    `(progn
       (defvar ,guard nil)
       (make-local-variable ',guard)
       (unless (buffer-local-value ',guard (current-buffer))
         (plet (((buffer-local-value ',guard (current-buffer)) t))
           ,@body)))))


(provide 'gurd-macros)
