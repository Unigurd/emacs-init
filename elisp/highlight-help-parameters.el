;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)


;; TODO: cleanup
;; TODO: In info, match on lower param names in the definition line and nowhere else.
;; TODO: ARGS in `use-package'

(defgroup hhp nil "Customization group for `hhp-mode'"
  :group 'help
  :version "29.2")

(defface hhp-face0 '((default :foreground "#b3b14b")) "Yellow" :group 'hhp)
(defface hhp-face1 '((default :foreground "#bd62bc")) "Purple" :group 'hhp)
(defface hhp-face2 '((default :foreground "#c98516")) "Orange" :group 'hhp)
(defface hhp-face3 '((default :foreground "#6665d8")) "Blue" :group 'hhp)
(defface hhp-face4 '((default :foreground "#75c23e")) "Green" :group 'hhp)
(defface hhp-face5 '((default :foreground "#5fb8b2")) "Turqoise" :group 'hhp)
(defcustom hhp-faces '(hhp-face0 hhp-face1 hhp-face2 hhp-face3 hhp-face4 hhp-face5)
  "Re-enable `hhp-mode' after changing this."
  :group 'hhp
  :type '(repeat face))

(defvar hhp-active-font-lock-keywords nil
  "Save the keywords actively used with font lock verbatim so they
can be removed even if `hhp-faces' has ben modified in the meantime")
(make-variable-buffer-local 'hhp-active-font-lock-keywords)

(defvar hhp-font-lock-state nil
  "State used by hhp when font locking.")
(make-variable-buffer-local 'hhp-font-lock-state)

(defvar hhp-find-parameters-function nil
  "Function specific to the buffer contents used to find the parameter list.")
(make-variable-buffer-local 'hhp-find-parameters-function)

(defun hhp-get-symbol-name ()
  (goto-char (point-min))
  (let ((second-line (progn (forward-line) (point))))
    (goto-char (point-min))
    ;; If we're in the help for a key, the first line is like
    ;; KEY runs the command COMMAND
    ;; so we search for that and leave point at COMMAND.
    ;; If we're not in the help for a key, point is left at
    ;; the start where COMMAND is.
    (re-search-forward "runs the command" second-line t)
    (condition-case nil
        (let ((symbol (read (current-buffer))))
          (when (symbolp symbol)
            (symbol-name symbol)))
      (end-of-file nil))))

(defun hhp-find-call (symbol)
  (let* ((regexp (format "^(%s" (regexp-quote (symbol-name symbol)))))
    (goto-char (point-min))
    (re-search-forward regexp nil t)
    ))

(defun hhp-filter-parameters (params)
  "filter and format"
  (cl-remove nil (mapcar #'hhp-format-param-name params)))

(defun hhp-modulo (params n i)
  "Skip the first I parameters and then take only every n.
Don't know what to call it, I guess it's kinda like modulo.
Or division, rather."
  (cl-loop for param in (nthcdr i params) by (lambda (ys) (nthcdr n ys))
           collect param))

(defun hhp-get-parameters (name)
  "This docstrings mentions HOW PLACE and PROPS"
  (let* ((regexp (format "^(%s" (regexp-quote name))))
    (when (and (re-search-forward regexp nil t) (search-backward "(" nil t))
      (condition-case nil
          (mapcar #'symbol-name
                  (mapcan (lambda (s)
                            (if (sequencep s)
                                (seq-into s 'list)
                              (list s)))
                          (cdr (read (current-buffer)))))
        (end-of-file nil)))))

(defun hhp-format-param-name (param-name)
  "Format `param-name' or return NIL if it is not an actual parameter,
like \"&rest\" or \"...\". Formatting consists of upcasing and removing
trailing \"...\".
"
  (when (not (= ?& (aref param-name 0))) ; Argument keywords like &rest, etc.
    (let ((formatted (upcase (string-trim-right param-name (regexp-quote "...")))))
      ;; "..." is used like &rest and should be ignored.
      ;; If `param-name' was "..." then it has been formatted to ""
      ;; and should be removed.
      (when (not (string= "" formatted))
        formatted))))


(defun hhp-make-regexp (names all-names)
  "Make a regexp that matches any of NAMES but not any of ALL-NAMES."
  (let ((avoid-names (cl-remove-if (lambda (avoid)
                                     (or (-any (lambda (name)
                                                 (string-search avoid name))
                                               names)
                                         (-every (lambda (name)
                                                   (not (string-search name avoid)))
                                                 names)))
                                   all-names)))
    (format "%s\\|%s"
            (regexp-opt avoid-names "\\(?:")
            (regexp-opt names "\\(?1:"))))

(defun hhp-font-lock-state (limit n i)
  (let ((state (funcall hhp-find-parameters-function limit)))
    (when state
      (cl-destructuring-bind
          (raw-params state-limit) state
        (let* ((all-params (hhp-filter-parameters raw-params))
               (params (hhp-modulo all-params n i))
               (regexp (hhp-make-regexp params all-params)))
          (setf hhp-font-lock-state (list regexp state-limit))))))
  hhp-font-lock-state)

(defun hhp-info-definition-search (lower-limit upper-limit forward)
  (let ((def-regex "^ -- [a-zA-Z]+: ")
        (end-regex (format "^%s[^ \n]" (regexp-opt '("" " " "  " "   " "    ")))))
    (save-excursion
      ;; Make sure we're not in the middle of a definition so neither a forward
      ;; or a backward search will find it.
      (end-of-line)
      (when-let ((def-start (progn (re-search-forward def-regex lower-limit t (if forward 1 -1))
                                   (beginning-of-line)
                                   (point)))
                 (def-end (progn (forward-line)
                                 (if (not (re-search-forward end-regex upper-limit t))
                                     upper-limit
                                   (forward-line -1)
                                   (end-of-line)
                                   (point)))))
        (list def-start def-end)))))

(defun hhp-info-definition-region (limit)
  (if-let ((rear-def-region (hhp-info-definition-search (point-min) limit nil)))
      (when (<= (car rear-def-region) (point) (cadr rear-def-region))
        rear-def-region)
    (hhp-info-definition-search limit limit t)))

(defun hhp-find-info-parameters (&optional limit)
  ;; parameters might be on multiple lines like for `count-screen-lines'
  (pcase (hhp-info-definition-region limit)
    (`(,start ,end)
     (message "start: %s end: %s" start end)
     (prog1 (list (mapcar #'symbol-name
                          (cdr (car (read-from-string
                                     (let ((sexpr (format "(%s)" (save-excursion
                                                                   (buffer-substring-no-properties
                                                                    (progn (goto-char start)
                                                                           (re-search-forward ": "))
                                                                    (progn (end-of-line) (point)))))))
                                       (message "sexpr: %s" sexpr)
                                       sexpr)))))
                  end)
       (message "(< point=%s start=%s)" (point) start)
       (when (< (point) start)
         (goto-char start))))
    ('() (message "no region")
     nil)))

(defun hhp-find-help-parameters (_)
  (list (save-excursion (hhp-get-parameters (hhp-get-symbol-name)))
        nil))

(defun hhp-fontify (limit n i &optional rec)
  (message "\nhhp-fontify")
  (message "point: %s" (point))
  (message "rec: %s" rec)
  (let ((state (or hhp-font-lock-state
                   (hhp-font-lock-state limit n i))))
    (message "state: %s" state)
    (when state
      (cl-destructuring-bind
          (regexp state-limit) state
        (let* ((current-limit (min (or state-limit limit) limit))
               (matchp (let ((case-fold-search nil))
                         (re-search-forward-group regexp 1 current-limit t))))
          (message "matchp: %s" matchp)
          (if matchp
              matchp
            (setf hhp-font-lock-state nil)
            (goto-char (1+ current-limit))
            (unless rec                 ; Only recurse once
              (hhp-fontify limit n i t))))))))

(defun re-search-forward-group (regexp subexpr &optional bound no-error count)
  "Belongs elsewhere."
  (let ((matchp nil))
    (while (and (setf matchp (re-search-forward regexp bound no-error count))
                (not (match-beginning subexpr))))

    (when matchp (match-beginning subexpr))))

(defun hhp-font-lock-keywords ()
  (let ((n (length hhp-faces)))
    (cl-loop for face in hhp-faces
             for i from 0
             collect `((lambda (limit) (hhp-fontify limit ,n ,i)) . (1 ',face)))))

(defun hhp-remove-font-lock-keywords ()
  (font-lock-remove-keywords nil (or hhp-active-font-lock-keywords (hhp-font-lock-keywords)))
  (setf hhp-active-font-lock-keywords nil))

(defun hhp-add-font-lock-keywords ()
  (hhp-remove-font-lock-keywords)
  (setf hhp-active-font-lock-keywords (hhp-font-lock-keywords))
  (font-lock-add-keywords nil hhp-active-font-lock-keywords))


(define-minor-mode highlight-help-parameters-mode
  "hej"
  :interactive (help-mode)
  (if highlight-help-parameters-mode
      (progn (setf hhp-find-parameters-function #'hhp-find-help-parameters)
             (hhp-add-font-lock-keywords))
    (hhp-remove-font-lock-keywords))
  (font-lock-flush))

(define-minor-mode highlight-info-parameters-mode
  "hej"
  :interactive (Info-mode)
  (if highlight-info-parameters-mode
      (progn (setf hhp-find-parameters-function #'hhp-find-info-parameters)
             (hhp-add-font-lock-keywords))
    (hhp-remove-font-lock-keywords))
  (font-lock-flush))

(provide 'highlight-help-parameters)
