;; -*- lexical-binding: t -*-

(require 'eieio)

(defclass gurd-req-param () ()
  "Class that signals an unbound-slot error if any of it's slots with
an initarg is unbound after initialization. Use an initform to provide
a default value for optional slots.")

(cl-defmethod initialize-instance :after ((self gurd-req-param) _)
  (let ((class (eieio--object-class self)))
    (dolist (initarg-pair (eieio--class-initarg-tuples class))
      (slot-value self (cdr initarg-pair)))))

(provide 'object-utils)
