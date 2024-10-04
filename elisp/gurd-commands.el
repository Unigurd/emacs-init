;;; -*- lexical-binding: t -*-

(require 'cl-lib)

(defun gurd-transform-region (find-region transform)
  (save-excursion
    (cl-destructuring-bind (start . end) (funcall find-region)
      (let* ((region-string (buffer-substring start end))
             (sexp (car (read-from-string region-string)))
             (new-region (funcall transform sexp)))
        (if (not (char-or-string-p new-region))
            (signal 'wrong-type-argument '(char-or-string-p t))
          (delete-region start end)
          (insert new-region))))))


(defun gurd-multiline ()
  "Deletes comments too"
  (interactive)
  (gurd-transform-region
   (lambda ()
     (cons (progn (end-of-line)
                  (beginning-of-defun)
                  (point))
           (progn (end-of-defun)
                  (point))))
   (lambda (sexp) (pp sexp (lambda (_) nil)))))

(defun gurd-macroexpand-defun (prefix)
  (interactive "P")
  (message "prefix: %s " prefix)
  (gurd-transform-region
   (lambda ()
     (cons (progn (end-of-line)
                  (beginning-of-defun)
                  (point))
           (progn (end-of-defun)
                  (point))))
   (lambda (sexp) (pp (funcall (if prefix #'macroexpand-all #'macroexpand) sexp)
                      (lambda (_) nil)))))

(defun gurd-macroexpand-sexp (prefix)
  (interactive "P")
  (message "prefix: %s " prefix)
  (gurd-transform-region
   (lambda ()
     ;; If point is before an opening parenthesis,
     ;; (bound-of-thing-at-point 'list) thinks the list at point
     ;; is the enclosing list, not the one right after point.
     ;; While that is sensible, we don't want that, and so we
     ;; do the following trick to get the desired behavior.
     (let ((list1 (bounds-of-thing-at-point 'list))
           (list2 (progn (forward-char) (bounds-of-thing-at-point 'list))))
       (cons (max (car list1) (car list2))
             (min (cdr list1) (cdr list2)))))
   (lambda (sexp) (pp (funcall (if prefix #'macroexpand-all #'macroexpand) sexp)
                      (lambda (_) nil)))))




(defun gurd-updir ()
  "Deletes topmost directory or file at or before point."
  ;; TODO: gracefully handle TRAMP prefix
  (interactive)
  (let ((end (point))
        (start (progn
                 ;; I believe "/" is the directory in emacs regardless of operating system
                 ;; and that it cannot be in a file name in linux or windows, so this should
                 ;; be correct.
                 (when (string= "/" (buffer-substring (- (point) 1) (point)))
                   (backward-char))
                 (skip-chars-backward
                  "^/"
                  (or (previous-single-property-change (point) 'read-only (current-buffer))
                      ;; First expression is nil when there is no property change before point,
                      ;; that is, when everything before is read-only. So we return (point) so we
                      ;; don't skip backwards at all.
                      (point)))
                 (point))))
    (delete-region start end)))

(defun gurd-normalize-path (path)
  ;; TODO: handle . and ..
  (cl-labels ((gurd--normalize-path
                (split-path &optional acc)
                (pcase split-path
                  (`("" ,child . ,rest)
                   (gurd--normalize-path (cons child rest) (list "")))
                  (`("~" ,child . ,rest)
                   (gurd--normalize-path (cons child rest) (list "~")))
                  (`("." ,child . ,rest)
                   (gurd--normalize-path (cons child rest) acc))
                  ;; (`(".." ,child . ,rest)
                  ;;  (pcase acc
                  ;;    (`("" . _) (gurd--normalize-path (cons child rest) acc))
                  ;;    (`())))
                  (`(,current ,child . ,rest)
                   (gurd--normalize-path (cons child rest) (cons current acc)))
                  (_
                   (reverse (append split-path acc))))))
    ;; I believe emacs always uses "/" as directory separator,
    ;; and that "/" cannot be part of a file name on linux and windows,
    ;; so I think it's safe to split on it, and do so without worrying
    ;; about escaping.
    (let ((tramp-prefix "")
          (local-path path))
      (when (string-match "^/[^/:]*:[^/:]*:" path)
        (setf tramp-prefix (match-string 0 path)
              local-path (substring path (match-end 0))))
      (concat tramp-prefix (string-join (gurd--normalize-path (split-string local-path "/")) "/")))))

(ert-deftest gurd-normalize-path ()
  (should (string= "/" (gurd-normalize-path "/")))
  (should (string= "" (gurd-normalize-path "")))
  (should (string= "~/d/e/f" (gurd-normalize-path "~/a/b/c/~/d/e/f")))
  (should (string= "~" (gurd-normalize-path "~")))
  (should (string= "~/e/f/" (gurd-normalize-path "~/a/b//c/d/~/e/f/")))
  (should (string= "/e/f" (gurd-normalize-path "/a/b/~/c/d//e/f")))
  (should (string= "/ssh:user@host:~/e/f/" (gurd-normalize-path "/ssh:user@host:~/a/b//c/d/~/e/f/")))
  (should (string= "/ssh:user@host:/e/f" (gurd-normalize-path "/ssh:user@host:/a/b/~/c/d//e/f")))
  (should (string= "." (gurd-normalize-path ".")))
  (should (string= "." (gurd-normalize-path "././././.")))
  (should (string= "" (gurd-normalize-path "./././././")))
  (should (string= "/." (gurd-normalize-path "/././././.")))
  (should (string= "/" (gurd-normalize-path "/./././././"))))


(defvar gurd-invoked-by-find-file)
(make-variable-buffer-local 'gurd-invoked-by-find-file)

(defun gurd-find-file-invoked-advice (function &rest args)
  (minibuffer-with-setup-hook
      (lambda () (setf gurd-invoked-by-find-file t))
    (apply function args)))

(advice-add #'read-file-name :around #'gurd-find-file-invoked-advice)


(defun gurd-minibuffer-normalize-path ()
  (interactive)
  ;; Only run if invoked by `find-file'
  (when gurd-invoked-by-find-file
    (let* ((start-of-path
            (or (previous-single-property-change (point) 'read-only (current-buffer))
                ;; First expression is nil when there is no property change before point,
                ;; that is, when everything before is read-only. So we return (point) so we
                ;; don't skip backwards at all.
                (point)))
           (normalized (gurd-normalize-path (buffer-substring start-of-path (point)))))
      (delete-region start-of-path (point))
      (insert normalized))))


(defun gurd-kill-other-buffer ()
  "Kill buffer in other window."
  (interactive)
  (kill-buffer (window-buffer (next-window))))


;; Emacs Lisp Elisp
(defun earmuffs (&optional pos)
  "Transforms SOME-SYMBOL to *SOME-SYMBOL* or the other way around.
Uses the symbol at *POS* if it is non-nil and symbol at point otherwise."
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (cl-destructuring-bind (start . end) (bounds-of-thing-at-point 'symbol)
      (if (and (eql ?* (char-after start))
               (eql ?* (char-before end)))
          (progn (goto-char end)
                 (delete-char -1)
                 (goto-char start)
                 (delete-char 1))
        (goto-char end)
        (insert-char ?*)
        (goto-char start)
        (insert-char ?*)))))


(provide 'gurd-commands)
