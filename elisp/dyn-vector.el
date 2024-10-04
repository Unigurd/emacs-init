; -*- lexical-binding: t -*-

(require 'object-utils)

(cl-defgeneric gurd-equal (a b))

(cl-defgeneric gurd-aref (thing idx))

(cl-defgeneric gurd-aset (thing idx newelt))

(cl-defgeneric gurd-length (thing))

(cl-defgeneric gurd-to-vector (thing))

(cl-defgeneric gurd-append (thing seq))

(cl-defgeneric gurd-append! (thing seq))

(cl-defgeneric gurd-vector-extend (dyn-vector &rest elts))

(cl-defgeneric gurd-vector-abridge (thing n))

(cl-defgeneric gurd-copy (thing))


(cl-defmethod gurd-equal (a b)
  (equal a b))

;; signal correct error on wrong type thing/idx?
(cl-defmethod gurd-aref (thing idx)
  (aref thing idx))

(cl-defmethod gurd-aset (thing idx newelt)
  (aset thing idx newelt))

(cl-defmethod gurd-length (thing)
  (length thing))

(cl-defmethod gurd-to-vector (thing)
  (vconcat thing))

(defclass gurd-vector (gurd-req-param)
  (vec          ; :LENGTH and :INIT are parameters used to compute VEC
   ))

(cl-defmethod initialize-instance ((self gurd-vector) args)
  (let* ((init-foundp nil)
         (init (plist-get args :init (lambda (a b) (when (eq a b) (setf init-foundp t)))))
         (length (plist-get args :length)))
    (cond ((and length init-foundp) (setf (slot-value self 'vec) (make-vector length init)))
          ((not init-foundp) (error "INITIALIZE-INSTANCE required key-word param :INIT is missing"))
          (t (error "INITIALIZE-INSTANCE required key-word param :LENGTH is missing")))))

(cl-defmethod gurd-copy ((self gurd-vector))
  (let* ((length (gurd-length self))
         (new (make-instance (eieio-object-class self)
                             :length length
                             :init nil)))
    (dotimes (i length)
      (gurd-aset new i (gurd-aref self i)))
    new))




(cl-defmethod gurd-to-vector ((thing gurd-vector))
  (let* ((length (gurd-length thing))
         (vector (make-vector length nil)))
    (dotimes (i length)
      (aset vector i (gurd-aref thing i)))
    vector))

(cl-defmethod gurd-append (self other)
  (seq-concatenate 'vector self (gurd-to-vector other)))


(cl-defmethod gurd-equal ((a gurd-vector) (b gurd-vector))
  (or (eq a b)
      (and (= (gurd-length a) (gurd-length b))
           (let ((bool t))
             (cl-loop for i to (- (gurd-length a) 1)
                      while (setf bool (and bool (gurd-equal (gurd-aref a i)
                                                             (gurd-aref b i)))))
             bool))))

(cl-defmethod gurd-aref ((self gurd-vector) idx)
  (aref (slot-value self 'vec) idx))

(cl-defmethod gurd-aset ((self gurd-vector) idx newelt)
  (aset (slot-value self 'vec) idx newelt))

(cl-defmethod gurd-length ((self gurd-vector))
  (length (slot-value self 'vec)))

(cl-defmethod gurd-append ((self gurd-vector) other)
  (let* ((self-length (gurd-length self))
         (other-length (gurd-length other))
         (length (+ self-length other-length))
         (new (make-instance (eieio-object-class self) :length length :init nil)))
    (dotimes (i self-length)
      (gurd-aset new i (gurd-aref self i)))
    (dotimes (i other-length)
      (let ((j (+ i self-length)))
        (gurd-aset new j (gurd-aref other i))))
    new))

(defun gurd-vector-from-vector (vector)
  (gurd-append (gurd-vector :length 0 :init nil) vector))


(defclass gurd-growable-vector (gurd-vector)
  ((filled :initform 0)
   (growth-factor :initarg :growth-factor
                  :initform 2)))

(cl-defgeneric gurd-growable-vector--make-room (gurd-growable-vector n))

(cl-defmethod initialize-instance ((self gurd-growable-vector) args)
  (let* ((init-foundp nil)
         (init (plist-get args :init (lambda (a b) (when (eq a b) (setf init-foundp t)))))
         (length (plist-get args :length)))
    (cond ((and length init-foundp)
           (setf (slot-value self 'vec) (make-vector length init)
                 (slot-value self 'filled) length))
          (length
           (setf (slot-value self 'vec) (make-vector length nil)))
          (init-foundp
           (error
            "INITIALIZE-INSTANCE key-word param :INIT should only be supplied along with :LENGTH"))
          (t (setf (slot-value self 'vec) (make-vector 0 nil))))))

(cl-defmethod gurd-aref :before ((self gurd-growable-vector) idx)
  (unless (< idx (slot-value self 'filled))
    (signal 'args-out-of-range (list self idx))))

(cl-defmethod gurd-aset :before ((self gurd-growable-vector) idx _)
  (unless (< idx (slot-value self 'filled))
    (signal 'args-out-of-range (list self idx))))

(cl-defmethod gurd-length ((self gurd-growable-vector))
  (slot-value self 'filled))

(cl-defmethod gurd-growable-vector--make-room ((self gurd-growable-vector) n)
  (let* ((filled (slot-value self 'filled))
         (new-filled (+ filled n))
         (growth-factor (slot-value self 'growth-factor))
         (vec (slot-value self 'vec))
         (length (length vec)))
    (when (< length new-filled)
      (let ((new-length (if (= length 0) growth-factor length)))
        (while (< new-length new-filled)
          (setf new-length (* new-length growth-factor)))
        ;; Convert new length to int
        (let ((new-vec (make-vector (ceiling new-length) nil)))
          (dotimes (idx length)
            (aset new-vec idx (aref vec idx)))
          (setf (slot-value self 'vec) new-vec))))))

(cl-defmethod gurd-append! ((self gurd-growable-vector) seq)
  (unless (seqp seq) (signal 'cl-no-applicable-method (list self seq)))
  (gurd-growable-vector--make-room self (length seq))
  (let ((vec (slot-value self 'vec))
        (filled (slot-value self 'filled))
        (seq-len (length seq)))
    ;; Doesn't work when seq is a gurd-vector
    (cl-loop for idx from filled
             for elt being the elements of seq
             do (setf (aref vec idx) elt))
    (cl-incf (slot-value self 'filled) seq-len)))

(cl-defmethod gurd-append! ((self gurd-growable-vector) (seq gurd-vector))
  (gurd-append! self (gurd-to-vector seq)))

(cl-defmethod gurd-append! ((self gurd-growable-vector) (other gurd-growable-vector))
  (gurd-append! self (slot-value other 'vec)))

(cl-defmethod gurd-vector-extend ((self gurd-growable-vector) &rest elts)
  (gurd-append! self elts))

(defun gurd-growable-vector-from-vector (vector)
  (gurd-append (gurd-growable-vector) vector))

(defclass gurd-shrinkable-vector (gurd-vector)
  ((filled)
   (shrink-factor :initarg :shrink-factor
                  :initform 0.25)
   (extra-space-factor :initarg :extra-space-factor
                       :initform 2)))

(cl-defmethod initialize-instance ((self gurd-shrinkable-vector) args)
  (cl-call-next-method self args)
  (setf (slot-value self 'filled) (length (slot-value self 'vec))))

(cl-defmethod gurd-shrinkable-vector-from-vector (vector)
  (gurd-append (gurd-shrinkable-vector :length 0 :init nil) vector))

(cl-defmethod gurd-length ((self gurd-shrinkable-vector))
  (slot-value self 'filled))

(cl-defmethod gurd-aref :before ((self gurd-shrinkable-vector) idx)
  (unless (< idx (slot-value self 'filled))
    (signal 'args-out-of-range (list self idx))))

(cl-defmethod gurd-aset :before ((self gurd-shrinkable-vector) idx _)
  (unless (< idx (slot-value self 'filled))
    (signal 'args-out-of-range (list self idx))))

(cl-defmethod gurd-shrinkable-vector--take-room ((self gurd-shrinkable-vector))
  (let* ((filled (slot-value self 'filled))
         (shrink-factor (slot-value self 'shrink-factor))
         (extra-space-factor (slot-value self 'extra-space-factor))
         (vec (slot-value self 'vec))
         (total-length (length vec)))
    (when (and (/= 0 total-length) (< (/ filled total-length) shrink-factor))
      (let ((new-vec (make-vector (ceiling (* filled extra-space-factor)) nil)))
        (dotimes (idx filled)
          (aset new-vec idx (aref vec idx)))
        (setf (slot-value self 'vec) new-vec)))))

(cl-defmethod gurd-vector-abridge ((self gurd-shrinkable-vector) n)
  (setf (slot-value self 'filled) (max 0 (- (slot-value self 'filled) n)))
  (gurd-shrinkable-vector--take-room self)
  nil)


(defclass gurd-dynamic-vector (gurd-growable-vector gurd-shrinkable-vector)
  ())

(cl-defmethod gurd-dynamic-vector-from-vector (vec)
  (gurd-append (gurd-dynamic-vector) vec))


(provide 'dyn-vector)
