
(defun gurd-plist-get (type l)
  (cond ((null l) nil)
        ((equal type :properties) (gurd-plist-get t l))
        ((equal type :values)    (gurd-plist-get nil l))
        ((null type) (gurd-plist-get (not type) (cdr l)))
        (t (cons (car l) (gurd-plist-get (not type) (cdr l))))))


(defun gurd-plist-map (f plist)
  (cond ((null plist) nil)
        ((and (consp plist) (consp (cdr plist)))
         (cons (car plist) (cons (funcall f (cadr plist)) (gurd-plist-map f (cddr plist)))))
        (t (error "gurd-plist-map given malformed plist"))))

(defun gurd-plist-get-with-default (plist prop default)
  (let ((val (plist-get plist prop)))
    (if (null val)
        default
      val)))

(provide 'gurd-plist)
