;;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'geiser-eval)
(require 'geiser-mode)

    ;; Functions for converting checks to tests in scheme.
(defun gurd-scheme-result (struc)
  "Return the result of a scheme computation
represented by STRUC or nil if it failed."
  (and (consp struc)
       (consp (car struc))
       (eq 'result (caar struc))
       (cadar struc)))

(defun gurd-geiser-check->test (check start end)
  "Convert a scheme (check ...) expressions into a (test name ...) expression.
CHECK is the string representing the check,and it should be located in
the buffer from START to END. name is provided interactively."
  ;; Ensure (check ...) succeeds
  (let ((check-result (geiser-eval--send/wait `(:eval (:scm ,check)) 500)))
    (if (not (gurd-scheme-result check-result))
        (message "Check failed")
      ;; Read name of test from minibuffer
      ;; and convert check to test
      (let* ((name (read-from-minibuffer "Test name: "))
             (test (gurd-scheme-result
                    (geiser-eval--send/wait
                     `(:eval (:scm ,(format "(check->test '%s '%s)"
                                            name
                                            check)))))))
        ;; Ensure conversion succeeded
        (if (not test)
            (message "Conversion to test failed")
          ;; Insert test
          (delete-region start end)
          (insert (concat test "\n"))
          (geiser-eval-definition))))))

(defun gurd-geiser--send-string (compile str and-go wrap &optional nomsg)
  (let* ((wrapped (if wrap (geiser-debug--wrap-region str) str))
         (code `(,(if compile :comp :eval) (:scm ,wrapped)))
         (cont (lambda (ret)
                 (let ((res (geiser-eval--retort-result-str ret nil))
                       (err (geiser-eval--retort-error ret))
                       (scstr (geiser-syntax--scheme-str str)))
                   (when and-go (funcall and-go))
                   (when (not err)
                     ;; Commented because I don't know what
                     ;; it does but seems unnecessary.
                     ;; (save-excursion
                     ;;   (goto-char (/ (+ end start) 2))
                     ;;   (geiser-autodoc--clean-cache))
                     (unless nomsg
                       (save-match-data
                         (when (string-match "\\(?:[ \t\n\r]+\\)\\'" res)
                           (setq res (replace-match "" t t res))))
                       (message "%s" res)))
                   (geiser-debug--display-retort scstr ret res)))))
    (geiser-eval--send code cont (current-buffer))))

(defvar gurd-geiser-minibuffer-history nil
  "Minibuffer history used in gurd-geiser--send-minibuffer")

(defun gurd-geiser--send-minibuffer (str)
  "Stolen from geiser-debug--send-region, but sends from minibuffer instead."
  (interactive (list (read-string "Eval: " nil 'gurd-geiser-minibuffer-history)))
  (gurd-geiser--send-string nil str nil t nil))

(defun gurd-geiser-test-dwim ()
  "Perform one of these actions depending on context.
If in a check expression, call GURD-GEISER-CHECK->TEST.
If in a define expression the test with the same name as the defined object.
If in a define-test expression, run that test.
If in a test expression, just run that.
Otherwise prompt for a test to run."
  ;; TODO: see if geiser can prettify generated test definition.
  ;; TODO: think about whether prefix arg should run (test-all).
  ;; TODO: Save test when run on a define-test.
  (interactive)
  (save-excursion
    ;; Get start and end of top-level expression
    (let ((end (progn (end-of-defun) (point)))
          (start (progn (beginning-of-defun) (point))))
      ;; Check for balanced parens
      (save-restriction
        (narrow-to-region start end)
        (check-parens))
      (let* ((str (buffer-substring-no-properties start end))
             (scm-car (gurd-scheme-result (geiser-eval--send/wait `(:eval (:scm ,(concat "(car '" str ")")) 500)))))
        ;; Ensure in (check ...) expression
        (cond ((string= "check" scm-car)
               (gurd-geiser-check->test str start end))
              ((or (string= "define" scm-car)
                   (string= "define-test" scm-car))
               (let ((test-name (gurd-scheme-result
                                 (geiser-eval--send/wait
                                  `(:eval (:scm ,(concat "(defined-name '" str ")"))) 500))))
                 (if test-name
                     (gurd-geiser--send-string nil (concat "(test '" test-name ")") nil nil)
                   (message "Malformed define/define-test."))))
              ((string= "test" scm-car)
               (gurd-geiser--send-string nil str nil nil))
              (t
               (gurd-geiser--send-string nil (concat "(test '" (read-string "Run test: " nil nil) ")") nil nil)))))))


(defun gurd-geiser-restart-from-file (buf-name)
  (interactive "b")
  (let ((source-path (buffer-file-name (get-buffer buf-name))))
    (cl-map 'list
         (lambda (some-buffer)
           (when (and (buffer-name some-buffer)
                      (string-match "\\*Geiser Guile REPL\\*.*"
                                    (buffer-name some-buffer)))
             (kill-buffer some-buffer)))
         (buffer-list))
    (geiser-repl-restart-repl)
    (geiser-compile--file-op (file-local-name (expand-file-name source-path)) nil "Loading")))


(provide 'gurd-geiser)
