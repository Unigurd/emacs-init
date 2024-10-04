;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun equal* (&rest args)
  "EQUAL but for any amount of arguments"
  (if args
      (let ((result t)
            (elm (pop args)))
        (while (and result args)
          (setf result (equal elm (car args))
                elm (pop args)))
        result)
    t))

(defun flatmap (fun &rest lists)
  (unless lists (signal 'wrong-number-of-arguments '(flatmap 1)))
  (apply 'append (apply 'cl-mapcar fun lists)))

(defun gurd-factorial (n)
  (cl-labels ((aux (n acc)
                (if (<= n 1)
                    acc
                  (aux (- n 1) (* acc n)))))
    (aux n 1)))

(defun gurd-choose (n k)
  (/ (gurd-factorial n)
     (gurd-factorial k)
     (gurd-factorial (- n k))))

(provide 'gurd-utils)
