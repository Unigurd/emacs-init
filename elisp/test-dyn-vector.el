; -*- lexical-binding: t -*-

(require 'propel)
(require 'dyn-vector)

(defun propel-gurd-vector (elm)
  (propel-map 'gurd-vector-from-vector 'gurd-to-vector (propel-vector elm)))

(defun propel-gurd-growable-vector (elm)
  (propel-map (lambda (vec)
                (let ((gv (gurd-growable-vector)))
                  (gurd-append! gv vec)
                  gv))
              'gurd-to-vector
              (propel-vector elm)))

(defun propel-gurd-shrinkable-vector (elm)
  (propel :gen (propel-gen-do
                ,(vec (propel-sized (lambda (size)
                                      (lambda (seed _)
                                        (propel-gencall (propel-gen-vector (propel-get-gen elm) size)
                                                        seed
                                                        size)))))
                ,(n propel-gen-natural)
                (let ((gv (gurd-shrinkable-vector-from-vector vec)))
                  (gurd-vector-abridge gv n)
                  (propel-return gv)))
          :shrink (compose (propel-shrink-sequence (propel-get-shrink elm))
                           'gurd-to-vector)))

(defun propel-gurd-dynamic-vector (elm)
  (propel-map (lambda (vec)
                (let ((gv (gurd-dynamic-vector)))
                  (gurd-append! gv vec)
                  gv))
              'gurd-to-vector
              (propel-vector elm)))

(ert-deftest gurd-vector ()
  ;; length
  (should (forall ((length propel-natural))
            (= length (gurd-length (gurd-vector :length length :init nil)))))
  ;; aref / aset
  (should (forall ((element propel-integer))
            ;; length is not in the forall since it needs to be a nat > 0
            ;; which i haven't implemented yet
            (let* ((length 9)
                   (idx (random length))
                   (gv (gurd-vector :length length :init nil)))
              (gurd-aset gv idx element)
              (equal element (gurd-aref gv idx)))))
  ;; :init
  (should (forall ((init propel-integer))
            ;; length is not in the forall since it needs to be a nat > 0
            ;; which i haven't implemented yet
            (let* ((length 8)
                   (gv (gurd-vector :length length :init init)))
              (let ((b t))
                (dotimes (idx length)
                  (setf b (and b (equal init (gurd-aref gv idx)))))
                b))))
  ;; aref length 0
  (should-error (gurd-aref (gurd-vector :length 0 :init nil) 0)
                :type 'args-out-of-range)
  ;; aset length 0
  (should-error (gurd-aset (gurd-vector :length 0 :init nil) 0 1)
                :type 'args-out-of-range)
  ;; gurd-equal
  (let ((propel-number-of-tests 15))
    ;; After about 15 I figure all v1, v2 are not gonna be equal
    (forall ((v1 (propel-vector propel-bool))
             (v2 (propel-vector propel-bool)))
      (equal (equal v1 v2)
             (gurd-equal (gurd-vector-from-vector v1)
                         (gurd-vector-from-vector v2)))))
  ;; vec -> gurd-vec -> vec
  ;; gurd-to-vector
  ;; gurd-vector-from-vector
  (should (forall ((vector (propel-vector propel-integer)))
            (equal vector (gurd-to-vector (gurd-vector-from-vector vector)))))
  ;; gurd-vec -> vec -> gurd-vec
  ;; gurd-to-vector
  ;; gurd-vector-from-vector
  (should (forall ((gv1 (propel-gurd-vector propel-integer)))
            (let* ((gv2 (gurd-vector-from-vector (gurd-to-vector gv1))))
              (gurd-equal gv1 gv2))))
  ;; (gurd-append gurd-vector gurd-vector)
  (forall ((v1 (propel-vector propel-integer))
           (v2 (propel-vector propel-integer)))
    (gurd-equal (gurd-append (gurd-vector-from-vector v1)
                             (gurd-vector-from-vector v2))
                (gurd-vector-from-vector (seq-concatenate 'vector v1 v2))))
  ;; (gurd-append gurd-vector vector
  (forall ((v1 (propel-vector propel-integer))
           (v2 (propel-vector propel-integer)))
    (gurd-equal (gurd-append (gurd-vector-from-vector v1) v2)
                (gurd-vector-from-vector (seq-concatenate 'vector v1 v2))))
  ;; (gurd-append vector gurd-vector
  (forall ((v1 (propel-vector propel-integer))
           (v2 (propel-vector propel-integer)))
    (gurd-equal (gurd-append v1 (gurd-vector-from-vector v2))
                (gurd-vector-from-vector (seq-concatenate 'vector v1 v2))))
  ;; gurd-append = vconcat
  (forall ((v1 (propel-vector propel-integer))
           (v2 (propel-vector propel-integer)))
    (gurd-equal (gurd-append (gurd-append (gurd-vector :length 0 :init nil) v1) v2)
                (gurd-vector-from-vector (vconcat v1 v2))))
  ;; gurd-append associativity
  (forall ((v1 (propel-vector propel-integer))
           (v2 (propel-vector propel-integer)))
    (gurd-equal (gurd-append (gurd-append (gurd-vector :length 0 :init nil) v1) v2)
                (gurd-append (gurd-vector :length 0 :init nil)
                             (gurd-append (gurd-vector-from-vector v1)
                                          (gurd-vector-from-vector v2)))))
  ;; gurd-copy
  (forall ((gv1 (propel-gurd-vector propel-integer)))
    (let ((gv2 (gurd-copy gv1)))
      (and (gurd-equal gv1 gv2)
           (not (eq gv1 gv2)))))
  ;; append returns gurd-vector
  (forall ((gv (propel-gurd-vector propel-integer)))
    (eq 'gurd-vector (eieio-object-class (gurd-append gv (vector))))))


(ert-deftest gurd-growable-vector ()
  ;; :LENGTH is the length of the internal buffer, while the length
  ;; of the growable vector is the number of items in it.
  (should (= 0 (gurd-length (gurd-growable-vector :length 9))))
  ;; When :INIT is specified the vector should be filled with that element
  ;; and it's length should reflect that
  (should (let ((length 9))
            (= length (gurd-length (gurd-growable-vector :length length :init nil)))))
  ;; extend = append!
  (should (forall ((elts (propel-list propel-integer)))
            (let ((gv1 (gurd-growable-vector))
                  (gv2 (gurd-growable-vector)))
              (gurd-append! gv1 (vconcat elts))
              (apply 'gurd-vector-extend gv2 elts)
              (gurd-equal gv1 gv2))))
  ;; append = append!
  (should (forall ((elts (propel-vector propel-integer)))
            (let ((gv1 (gurd-growable-vector))
                  (gv2 (gurd-growable-vector)))
              (gurd-append! gv1 elts)
              (gurd-equal gv1 (gurd-append gv2 elts)))))
  ;; append = extend
  (should (forall ((elts (propel-list propel-integer)))
            (let ((gv1 (gurd-growable-vector))
                  (gv2 (gurd-growable-vector)))
              (apply 'gurd-vector-extend gv1 elts)
              (gurd-equal gv1 (gurd-append gv2 (vconcat elts))))))
  ;; append like vector append
  (should (forall ((gv1 (propel-gurd-growable-vector propel-integer))
                   (gv2 (propel-gurd-growable-vector propel-integer)))
            (equal (gurd-to-vector (gurd-append gv1 gv2))
                   (vconcat (gurd-to-vector gv1) (gurd-to-vector gv2)))))
  ;; vec -> growable -> vec
  (should (forall ((vec (propel-vector propel-integer)))
            (equal vec (gurd-to-vector (gurd-growable-vector-from-vector vec)))))
  ;; append associativity (kind of)
  (should (forall ((v1 (propel-vector propel-integer))
                   (v2 (propel-vector propel-integer))
                   (gv (propel-gurd-growable-vector propel-integer)))
            (gurd-equal (gurd-append (gurd-append gv v1) v2)
                        (gurd-append gv (vconcat v1 v2)))))
  ;; append! associativity (kind of)
  (should (forall ((v1 (propel-vector propel-integer))
                   (v2 (propel-vector propel-integer))
                   (gv1 (propel-gurd-growable-vector propel-integer)))
            (let ((gv2 (gurd-copy gv1)))
              (gurd-append! gv1 v1)
              (gurd-append! gv1 v2)
              (gurd-append! gv2 (vconcat v1 v2))
              (gurd-equal gv1 gv2))))
  ;; extend associativity (kind of)
  (should (forall ((v1 (propel-list propel-integer))
                   (v2 (propel-list propel-integer))
                   (gv1 (propel-gurd-growable-vector propel-integer)))
            (let ((gv2 (gurd-copy gv1)))
              (apply 'gurd-vector-extend gv1 v1)
              (apply 'gurd-vector-extend gv1 v2)
              (apply 'gurd-vector-extend gv2 (append v1 v2))
              (gurd-equal gv1 gv2))))
  ;; gurd-copy
  (should (forall ((gv1 (propel-gurd-growable-vector propel-integer)))
            (let ((gv2 (gurd-copy gv1)))
              (and (gurd-equal gv1 gv2)
                   (not (eq gv1 gv2))))))
  ;; append returns growable
  (forall ((gv (propel-gurd-growable-vector propel-integer)))
    (eq 'gurd-growable-vector (eieio-object-class (gurd-append gv (vector))))))

(ert-deftest gurd-shrinkable-vector ()
  ;; x = x
  (should (forall ((gv (propel-gurd-shrinkable-vector propel-integer)))
            (gurd-equal gv gv)))
  ;; vec -> shrinkable -> vec
  (should (forall ((vec (propel-vector propel-integer)))
            (equal vec (gurd-to-vector (gurd-shrinkable-vector-from-vector vec)))))
  (should (forall ((gv (propel-gurd-shrinkable-vector propel-integer)))
            (= (gurd-length gv) (length (gurd-to-vector gv)))))
  ;; append
  (should (forall ((gv1 (propel-gurd-shrinkable-vector propel-integer))
                   (gv2 (propel-gurd-shrinkable-vector propel-integer)))
            (equal (gurd-to-vector (gurd-append gv1 gv2))
                   (vconcat (gurd-to-vector gv1) (gurd-to-vector gv2)))))
  (should (forall ((gv (propel-gurd-shrinkable-vector propel-integer)))
            (gurd-equal gv (gurd-copy gv))))
  ;; length after abridge
  (should (forall ((gv (propel-gurd-shrinkable-vector propel-integer))
                   (n propel-natural))
            (let ((length (gurd-length gv)))
              (gurd-vector-abridge gv n)
              (= (max 0 (- length n))
                 (gurd-length gv)))))
  ;; gsv[i] is the same before and after abridge
  (should (forall ((gv (propel-gurd-shrinkable-vector propel-integer))
                   (n propel-natural)
                   (m propel-natural))
            (let ((new-length (max 0 (- (gurd-length gv) n))))
              (if (= 0 new-length)
                  t
                (let* ((i (mod m new-length))
                       (orig-elm (gurd-aref gv i)))
                  (gurd-vector-abridge gv n)
                  (= orig-elm (gurd-aref gv i))))))))


(ert-deftest gurd-dynamic-vector ()
  ;; :LENGTH is the length of the internal buffer, while the length
  ;; of the dynamic vector is the number of items in it.
  (should (= 0 (gurd-length (gurd-dynamic-vector :length 9))))
  ;; When :INIT is specified the vector should be filled with that element
  ;; and it's length should reflect that
  (should (let ((length 9))
            (= length (gurd-length (gurd-dynamic-vector :length length :init nil)))))
  ;; extend = append!
  (should (forall ((elts (propel-list propel-integer)))
            (let ((gv1 (gurd-dynamic-vector))
                  (gv2 (gurd-dynamic-vector)))
              (gurd-append! gv1 (vconcat elts))
              (apply 'gurd-vector-extend gv2 elts)
              (gurd-equal gv1 gv2))))
  ;; append = append!
  (should (forall ((elts (propel-vector propel-integer)))
            (let ((gv1 (gurd-dynamic-vector))
                  (gv2 (gurd-dynamic-vector)))
              (gurd-append! gv1 elts)
              (gurd-equal gv1 (gurd-append gv2 elts)))))
  ;; append = extend
  (should (forall ((elts (propel-list propel-integer)))
            (let ((gv1 (gurd-dynamic-vector))
                  (gv2 (gurd-dynamic-vector)))
              (apply 'gurd-vector-extend gv1 elts)
              (gurd-equal gv1 (gurd-append gv2 (vconcat elts))))))
  ;; append like vector append
  (should (forall ((gv1 (propel-gurd-dynamic-vector propel-integer))
                   (gv2 (propel-gurd-dynamic-vector propel-integer)))
            (equal (gurd-to-vector (gurd-append gv1 gv2))
                   (vconcat (gurd-to-vector gv1) (gurd-to-vector gv2)))))
  ;; vec -> dynamic -> vec
  (should (forall ((vec (propel-vector propel-integer)))
            (equal vec (gurd-to-vector (gurd-dynamic-vector-from-vector vec)))))
  ;; append associativity (kind of)
  (should (forall ((v1 (propel-vector propel-integer))
                   (v2 (propel-vector propel-integer))
                   (gv (propel-gurd-dynamic-vector propel-integer)))
            (gurd-equal (gurd-append (gurd-append gv v1) v2)
                        (gurd-append gv (vconcat v1 v2)))))
  ;; append! associativity (kind of)
  (should (forall ((v1 (propel-vector propel-integer))
                   (v2 (propel-vector propel-integer))
                   (gv1 (propel-gurd-dynamic-vector propel-integer)))
            (let ((gv2 (gurd-copy gv1)))
              (gurd-append! gv1 v1)
              (gurd-append! gv1 v2)
              (gurd-append! gv2 (vconcat v1 v2))
              (gurd-equal gv1 gv2))))
  ;; extend associativity (kind of)
  (should (forall ((v1 (propel-list propel-integer))
                   (v2 (propel-list propel-integer))
                   (gv1 (propel-gurd-dynamic-vector propel-integer)))
            (let ((gv2 (gurd-copy gv1)))
              (apply 'gurd-vector-extend gv1 v1)
              (apply 'gurd-vector-extend gv1 v2)
              (apply 'gurd-vector-extend gv2 (append v1 v2))
              (gurd-equal gv1 gv2))))
  ;; gurd-copy
  (should (forall ((gv1 (propel-gurd-dynamic-vector propel-integer)))
            (let ((gv2 (gurd-copy gv1)))
              (and (gurd-equal gv1 gv2)
                   (not (eq gv1 gv2))))))
  ;; append returns dynamic
  (forall ((gv (propel-gurd-dynamic-vector propel-integer)))
    (eq 'gurd-dynamic-vector (eieio-object-class (gurd-append gv (vector)))))
  ;; x = x
  (should (forall ((gv (propel-gurd-dynamic-vector propel-integer)))
            (gurd-equal gv gv)))
  ;; vec -> dynamic -> vec
  (should (forall ((vec (propel-vector propel-integer)))
            (equal vec (gurd-to-vector (gurd-dynamic-vector-from-vector vec)))))
  (should (forall ((gv (propel-gurd-dynamic-vector propel-integer)))
            (= (gurd-length gv) (length (gurd-to-vector gv)))))
  ;; append
  (should (forall ((gv1 (propel-gurd-dynamic-vector propel-integer))
                   (gv2 (propel-gurd-dynamic-vector propel-integer)))
            (equal (gurd-to-vector (gurd-append gv1 gv2))
                   (vconcat (gurd-to-vector gv1) (gurd-to-vector gv2)))))
  (should (forall ((gv (propel-gurd-dynamic-vector propel-integer)))
            (gurd-equal gv (gurd-copy gv))))
  ;; length after abridge
  (should (forall ((gv (propel-gurd-dynamic-vector propel-integer))
                   (n propel-natural))
            (let ((length (gurd-length gv)))
              (gurd-vector-abridge gv n)
              (= (max 0 (- length n))
                 (gurd-length gv)))))
  ;; gsv[i] is the same before and after abridge
  (should (forall ((gv (propel-gurd-dynamic-vector propel-integer))
                   (n propel-natural)
                   (m propel-natural))
            (let ((new-length (max 0 (- (gurd-length gv) n))))
              (if (= 0 new-length)
                  t
                (let* ((i (mod m new-length))
                       (orig-elm (gurd-aref gv i)))
                  (gurd-vector-abridge gv n)
                  (= orig-elm (gurd-aref gv i)))))))
  ;; abridge . append! = id
  (forall ((gv (propel-gurd-dynamic-vector propel-integer))
           (elms (propel-list propel-integer)))
    (let ((orig (gurd-copy gv)))
      (gurd-append! gv elms)
      (gurd-vector-abridge gv (length elms))
      (and (not (eq gv orig)) (gurd-equal orig gv))))
  ;; abridge . append! . abridge . append! = id
  (forall ((gv (propel-gurd-dynamic-vector propel-integer))
           (elms1 (propel-list propel-integer))
           (elms2 (propel-list propel-integer)))
    (let ((orig (gurd-copy gv)))
      (gurd-append! gv elms1)
      (gurd-vector-abridge gv (length elms1))
      (gurd-append! gv elms2)
      (gurd-vector-abridge gv (length elms2))
      (and (not (eq gv orig)) (gurd-equal orig gv)))))





