;;; -*- lexical-binding: t -*-

;;; Code:
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))

(setf custom-theme-directory (expand-file-name "custom-themes/" user-emacs-directory))

(load custom-file t)

(load-theme 'gurd t)


;;
;;:   PACKAGES
;;

;;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; use-package
(eval-when-compile
  ;; I think I only (require 'use-package) at compile time
  ;; because it's only a macro so no reason for it to stick around after
  ;; macro-expansion?
  (require 'use-package))

(add-to-list 'load-path "/home/gurd/emacs/")
(let ((default-directory "/home/gurd/emacs/"))
  (normal-top-level-add-subdirs-to-load-path))

(let ((default-directory
       (expand-file-name (file-name-as-directory (file-name-concat  user-emacs-directory "elisp" "")))))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))


;;
;;:   SETUP
;;



(setf display-raw-bytes-as-hex t)

;; Disable emacs startup-screen
(setq inhibit-startup-screen t)

;;Removes the bars
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;;Show line and column number in the mode line
(column-number-mode 1)

(setq sentence-end-double-space nil)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;;Stores backup files in ~/.emacs.d/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; displays the time and date in the mode line
;;(display-time)

(global-set-key (kbd "C-x K") 'kill-this-buffer)

;; Don't shorten the printed representation of evaluated lisp data
;; with ellipsis like (foo bar ...)
(setf eval-expression-print-length nil
      eval-expression-print-level nil)

(defun gurd-kill-other-buffer ()
  "Kill buffer in other window."
  (interactive)
  (kill-buffer (window-buffer (next-window))))

(global-set-key (kbd "C-S-x k") 'gurd-kill-other-buffer)

;;Overwrite selected content
(delete-selection-mode)

;; move to the window in the other direction of C-x o
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

;;C-S-D works as delete
(global-set-key (kbd "C-S-D") 'backward-delete-char-untabify)

;;highlighting matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; make ibuffer default buffer manager
(defalias 'list-buffers 'ibuffer)

;; easy keys to split window. Key based on ErgoEmacs keybinding
;; expand current pane
(global-set-key (kbd "C-M-3") 'delete-other-windows)
;; close current pane
(global-set-key (kbd "C-M-#") 'delete-window)
;; split pane left/right
(global-set-key (kbd "C-M-2") 'split-window-right)
;;  split pane top/bottom
(global-set-key (kbd "C-M-@") 'split-window-below)

;; show trailing whitespace
(setq-default show-trailing-whitespace t)

;; higher contrast (of what?)
(setq shr-color-visible-luminance-min 70)

;; hide trailing whitespace in selected modes
(defun my-hide-trailing-whitespace-maybe ()
  "Disable `show-trailing-whitespace' in selected modes."
  (when (derived-mode-p 'shell-mode
                        'eww-mode
                        'rc-irc-mode
                        'comint-mode
                        'erc-mode
                        'erc-list-mode
                        'term-mode
                        'compilation-mode) ; Should that be changed to special-mode?
    (setq show-trailing-whitespace nil)))

(add-hook 'after-change-major-mode-hook
          'my-hide-trailing-whitespace-maybe)

;; No need to confirm when reverting buffers
;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))


(global-set-key (kbd "C-c u") 'revert-buffer-no-confirm)


;; Make marks nicer
(setf set-mark-command-repeat-pop t
      mark-ring-max 32
      global-mark-ring-max 32)




;; Press f3 in shell-command to insert current file name
(define-key minibuffer-local-map [f3]
  (lambda() (interactive) (insert (buffer-file-name (nth 1 (buffer-list))))))

;; ;; Go up a directory from anywhere in a dired buffer
;; (defun up-dir ()
;;   (interactive)
;;   (find-file (concat (if (listp dired-directory) (car dired-directory) dired-directory) "/..")))
;; (defun up-dir-alternate ()
;;   (interactive)
;;   (find-alternate-file
;;    (concat (if (listp dired-directory) (car dired-directory) dired-directory) "/..")))
;; (bind-key (kbd "[") 'up-dir dired-mode-map)
;; (bind-key (kbd "{") 'up-dir-alternate dired-mode-map)
;; org mode

(use-package org
  :ensure t
  :config
  (bind-key (kbd "C-c l") 'org-store-link)
  (bind-key (kbd "C-c a") 'org-agenda)
  (bind-key (kbd "C-c c") 'org-capture)
  ;; Default file for capturing
  (setq org-default-notes-file (concat org-directory "/capture.org"))
  ;; The files in my global org thing
  (setq org-agenda-files (list "~/org/uni.org"
                               "~/org/adult.org"
                               "~/org/mailCalendar.org"))
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (org-babel-do-load-languages
   'org-babel-load-languages
   (add-to-list 'org-babel-load-languages '(haskell . t))))
(use-package ox-beamer)

(use-package latex
  :ensure auctex)

(use-package ox-latex
  :after (org ox-beamer)
  :config
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))
  (define-key org-beamer-mode-map (kbd "C-c C-b") ())
  (define-key org-beamer-mode-map (kbd "C-c b") 'org-beamer-select-environment))


(use-package org-ref
  :ensure t
  :config
  ;; (setq org-latex-pdf-process '("texi2dvi -p -b -V %f"))
  (setq org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf %f")))

(setq next-screen-context-lines 28)

(define-key global-map (kbd "C-c h") 'comment-region)


(let* ((version (progn
                  (string-match "GNU Emacs \\([0-9]+\\.[0-9]+\\)" (version))
                  (match-string 1 (version))))
       (c-source-dir (concat "~/.emacs.d/c-source/emacs-" version "/src")))
  (when (file-exists-p c-source-dir)
    (setq find-function-C-source-directory c-source-dir)))


;; email smtp
(setf mail-host-address "hotmail.com"
      user-mail-address "Sigurddam@hotmail.com")

(setf user-full-name "Sigurd Dam Sonniks")

(use-package flycheck)


(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;;
;;: gurd
;;


(defun hex-to-ascii (hex-string)
  "Convert string HEX-STRING to its ascii representation."
  (unless (= (mod (string-width hex-string) 2) 0)
    (error "Uneven amount of hex digits"))
  (let ((hex-to-num
         (lambda (hex-char)
           (pcase hex-char
             (48 0) (49 1) (50 2) (51 3) (52 4) (53 5) (54 6) (55 7) (56 8)
             (57 9) (97 10) (98 11) (99 12) (100 13) (101 14) (102 15)
             (_ (throw 'char-not-valid-hex nil)))))
        (rev-ascii-list nil))
    (dotimes (i (/ (string-width hex-string) 2))
      (setf rev-ascii-list
            (cons (+ (* 16 (funcall hex-to-num (aref hex-string (* 2 i))))
                     (funcall hex-to-num (aref hex-string (+ (* 2 i) 1))))
                  rev-ascii-list)))
    (concat (reverse rev-ascii-list))))

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

(defun gurd-map-these (p f l)
  (cond ((consp l)
         (cons (if (funcall p (car l)) (funcall f (car l)) (car l)) (gurd-map-these p f (cdr l))))
        ((null l) l)
        (t (signal 'wrong-type-argument (list 'listp l)))))

;; Minesweeper
(let* ((mines-dir "~/.emacs.d/git/mines/")
       (mines-source-file (concat mines-dir "mines.el"))
       (mines-compiled-file (concat mines-source-file "c")))
  (when (file-exists-p mines-source-file)
    (unless (or (file-exists-p (concat mines-dir "mines.elc"))
                (file-newer-than-file-p mines-source-file mines-compiled-file))
      (byte-compile-file mines-source-file))
    (load-file mines-compiled-file)))
(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "SPC") 'mines-sweep)))
(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "<tab>") 'mines-flag)))
(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "<down-mouse-1>") 'ignore)))
(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "<mouse-2>") 'ignore)))


;;
;;:   PACKAGES
;;

;; persistent-scratch doesn't get properly installed
;; with use-package
(unless (package-installed-p 'helm)
  (package-install 'helm))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-mode))

(use-package dired
  :config
  (defun xah-dired-mode-setup ()
    "Hides details in Dired. To be run as hook for `dired-mode'. From http://ergoemacs.org/emacs/emacs_dired_tips.html"
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'xah-dired-mode-setup))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package magit
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist
        `(("." . ,(file-name-concat user-emacs-directory ".cache" "undo-tree"))))
  (global-undo-tree-mode))

;; Evil-mode
(use-package evil
  :config
  (setf evil-default-state 'emacs
        evil-motion-state-modes nil)
  (add-to-list 'evil-buffer-regexps '("^COMMIT_EDITMSG$" . emacs) )
  (with-no-warnings ; silence `evil-global-set-key' not known to be defined
    (evil-global-set-key 'normal (kbd "<tab>") 'indent-for-tab-command)
    (evil-global-set-key 'insert (kbd "<tab>") 'indent-for-tab-command)
    (evil-set-initial-state 'prog-mode 'normal)
    (evil-set-initial-state 'text-mode 'normal))
  (evil-mode))


;; ace-window
(use-package ace-window
  :ensure t
  :config
  (bind-key (kbd "C-M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; rainbow
(use-package rainbow-blocks
  :ensure t)



(use-package rainbow-delimiters
  :ensure t
  :config
  ;; rainbow-delimiters automatisk paa
  (define-globalized-minor-mode my-global-rainbow-delimiters-mode rainbow-delimiters-mode
    (lambda () (rainbow-delimiters-mode 1)))
  (my-global-rainbow-delimiters-mode 1))

;; calendar
(use-package calfw
  :ensure t)

(use-package calfw-org
  :ensure t)

;;;;  both ways org-gcal sync
;;(load-file "~/.emacs.d/org-caldav/org-caldav.el")
;;(setq org-caldav-oauth2-client-id "801724761408-20jktro7tqi7it8ls39f6192fpulg7n9.apps.googleusercontent.com"
;;      org-caldav-oauth2-client-secret "KYX431MCLAjudoe_6FSm1rwh"
;;      org-caldav-calendar-id "sigurddam@gmail.com"
;;      org-caldav-url 'google
;;      org-caldav-id "801724761408-20jktro7tqi7it8ls39f6192fpulg7n9.apps.googleusercontent.com"
;;      org-caldav-files (list "~/org/uni.org"
;;                             "~/org/adult.org")
;;      org-caldav-inbox "~/org/mailCalendar.org"
;;      org-icalendar-timezone "Europe/Copenhagen")
;;change w3m user-agent to android
;; But why?

(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

;; Back when I didn't use the mouse :')
(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter " 🐭"
  :keymap (make-sparse-keymap))


(dolist (type '(mouse down-mouse drag-mouse
                      double-mouse triple-mouse))
  (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
    ;; Yes, I actually HAD to go up to 7 here.
    (dotimes (n 7)
      (let ((k (format "%s%s-%s" prefix type n)))
        (define-key disable-mouse-mode-map
          (vector (intern k)) #'ignore)))))

;;Smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/mode-width 'full)
  (sml/setup)
  (put 'narrow-to-page 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (put 'upcase-region 'disabled nil))

(use-package font-lock-studio)

(use-package ace-jump-mode
    :bind ("C-S-SPC" . ace-jump-char-mode)
    :config
    (setf ace-jump-mode-move-keys
          (list ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
                ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
                ?a ?s ?d ?f ?g ?h ?j ?k ?l
                ?z ?x ?c ?v ?b ?n ?m)))

(use-package paredit
  :hook (lisp-data-mode . paredit-mode))

(use-package evil-paredit
  :after (evil paredit)
  :hook (lisp-data-mode . evil-paredit-mode))


;; (use-package evil-lispy
;;   :config
;;   (define-advice lispy--insert (:around (wrapped-fun &rest args) no-ellipsis)
;;     "To avoid lispy replacing code with ..."
;;     (let ((print-level nil)
;;           (print-length nil))
;;       (apply wrapped-fun args)))
;;   :hook (lisp-data-mode . evil-lispy-mode))



(setq flycheck-standard-error-navigation nil)




;;
;;:   LANGUAGES
;;

(use-package nix-mode)

;; Haskell
(progn
  (use-package haskell-mode
    :config
    (setq haskell-tags-on-save t)
    (setq tags-revert-without-query t)
    (define-key haskell-mode-map (kbd "C-c j") 'haskell-mode-jump-to-def)
    (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
    (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
    ;; :hook
    ;; (haskell-mode . flycheck-haskell-setup)
    ;;(haskell-mode . haskell-collapse-mode)
    )

  (use-package haskell-cabal
    :config
    '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

  (use-package haskell-interactive-mode
    :after (haskell-mode)
    :config
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

  (use-package haskell-process
    :after (haskell-interactive-mode))

  ;;(define-key haskell-interactive-mode-map (kbd "C-`") 'haskell-interactive-bring)
  )

;; shell
(defvar sh-basic-offset 2)

;; Prolog
(progn
  (use-package ediprolog
    :config
    (global-set-key [f10] 'ediprolog-dwim)
    (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
    (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode)))

  (defun browse-swi-prolog (query)
    "Search Swi-Prolog site for QUERY."
    (interactive (list (let* ((default (thing-at-point 'word))
                              (message (format "Browse Swi-Prolog (default %s): " default)))
                         (read-string message nil nil default))))
    (browse-url (format "https://www.swi-prolog.org/search?for=%s" query))))



;;


;; Emacs Lisp Elisp
(defun earmuffs (&optional pos)
  "Transforms SOME-SYMBOL to *SOME-SYMBOL* or the other way around.
Uses the symbol at *POS* if it is non-nil and symbol at point otherwise."
  (interactive)
  (save-excursion
    (if pos (goto-char pos))
    (destructuring-bind (start . end) (bounds-of-thing-at-point 'symbol)
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

(define-key emacs-lisp-mode-map (kbd "C-c *") 'earmuffs)

(add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
(setq sentence-end-double-space nil)

;; Common Lisp
(use-package slime
  :config
  (let ((slime-file "~/quicklisp/slime-helper.el"))
    (when (file-exists-p slime-file)
      (load (expand-file-name "~/quicklisp/slime-helper.el"))))

  (defun gurd-slime-inspector-link-finder (limit)
    (let ((match (text-property-search-forward 'slime-part-number)))
      (when (and match (> limit (prop-match-end match)))
        (set-match-data (list (prop-match-beginning match) (prop-match-end match))))
      match))

  (font-lock-add-keywords 'slime-inspector-mode `((,#'gurd-slime-inspector-link-finder . font-lock-builtin-face)))

  (advice-add 'slime-sexp-at-point :filter-return
              (lambda (arg)
                "Prepend ' to read sexp if current-prefix-arg."
                ;; TODO: find way to check if some ancestor fun is called interactively
                (if current-prefix-arg
                    (concat "'" arg)
                  arg)))
  (setf slime-scratch-file "~/.emacs.d/slime-scratch")

  (add-hook 'lisp-mode-hook #'evil-lispy-mode)
  :bind
  ("C-c *" . earmuffs))

;; Replace "sbcl" with the path to your implementation
(setf inferior-lisp-program "sbcl")






;; scheme
(unless (package-installed-p 'geiser)
  (package-install 'geiser))
(use-package geiser
  :config
  (add-hook 'scheme-mode-hook #'evil-lispy-mode)
  ;; Manual specification of indentation of my macros.
  (mapc (lambda (m)
          (put m 'scheme-indent-function 1))
        '(data
          gen-stack-top
          dotimes
          define-lookup
          define-pattern-expander
          match
          ematch
          multi-match
          multi-ematch
          define-test
          check
          check!
          forall
          counter-example
          with-gensyms))

  (put 'with-sizes 'scheme-indent-function 2)

  ;; Functions for converting checks to tests in scheme.
  (defun gurd-scheme-result (struc)
    "Return the result of a scheme computation represented by STRUC or nil if it failed."
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
      (map 'list
           (lambda (some-buffer)
             (when (and (buffer-name some-buffer)
                        (string-match "\\*Geiser Guile REPL\\*.*"
                                      (buffer-name some-buffer)))
               (kill-buffer some-buffer)))
           (buffer-list))
      (geiser-repl-restart-repl)
      (geiser-compile--file-op (file-local-name (expand-file-name source-path)) nil "Loading")))


  :bind (;; :map geiser-mode-map
         ("C-c t" . gurd-geiser-test-dwim)
         ("C-c e" . gurd-geiser--send-minibuffer)
         ("C-c r" . gurd-geiser-restart-from-file)))


(unless (package-installed-p 'geiser-guile)
  (package-install 'geiser-guile))
(use-package geiser-guile
  :after geiser)



;; C
;; use spaces instead of tabs, at least in cc mode
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2) ; Assuming you want your tabs to be four spaces wide
(defvaralias 'c-basic-offset 'tab-width)

; c indentation is now 2 spaces, i think
(defvar c-default-style "bsd")
(setq c-basic-offset 2)

;; Java?
;;indents arguments to functions with long names better
(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)

;; Python
;; set python interpreter
(defvar python-shell-interpreter "python3")

;; arduino
(use-package arduino-mode
  :config
  (add-hook 'arduino-mode-hook #'flycheck-arduino-setup))



;; Disable disabling these commands
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'list-threads 'disabled nil)


(provide 'init)
;;; init.el ends here
