;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(unless package-archive-contents (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-time-24hr-format t)
 '(doc-view-continuous t)
 '(erc-hl-nicks-mode t)
 '(erc-nick "Unigurd")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(org-agenda-files
   '("~/org/learn.org" "~/.emacs.d/elpa/org-ref-20200606.1848/org-ref.org" "~/org/uni.org" "~/org/adult.org" "~/org/mailCalendar.org"))
 '(package-selected-packages
   '(evil-lispy haskell-process haskell-interactive-mode haskell-cabal wiki-summary haskell-mode arduino-mode flycheck slime cuda-mode lispy org-ref erc-hl-nicks use-package ace-window futhark-mode buffer-move ein pdf-tools transient magit evil dante intero ediprolog ## gnu-elpa-keyring-update oauth2 org-gcal calfw-org calfw cider rainbow-blocks rainbow-delimiters rainbow-mode markdown-mode projectile clojure-mode better-defaults smartparens w3m fsharp-mode))
 '(rainbow-delimiters-max-face-count 8)
 '(safe-local-variable-values '((flycheck-mode . t)))
 '(send-mail-function 'smtpmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "grey"))))
 '(fringe ((t (:background "black"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "dodger blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "magenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "light sky blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "peru"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "pale green")))))
(put 'narrow-to-region 'disabled nil)


;;
;;:   SETUP
;;

;; Disable emacs startup-screen
(setq inhibit-startup-screen t)

;;Removes the bars
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;;Show line and column number in the mode line
(column-number-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;;Stores backup files in ~/.emacs.d/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; displays the time and date in the mode line
;;(display-time)

(global-set-key (kbd "C-x K") 'kill-this-buffer)


(defun gurd-kill-other-buffer ()
  "Kill buffer in other window."
  (interactive)
  (kill-buffer (window-buffer (next-window))))

(global-set-key (kbd "C-S-x k") 'gurd-kill-other-buffer)

;; binds hippe-expand
(global-set-key "\M- " 'hippie-expand)

;; Displays battery percentage in the mode line
;;(display-battery-mode 1)

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
(global-set-key (kbd "M-2") 'delete-other-windows)
;; close current pane
(global-set-key (kbd "M-3") 'delete-window)
;; split pane left/right
(global-set-key (kbd "M-4") 'split-window-right)
;;  split pane top/bottom
(global-set-key (kbd "M-5") 'split-window-below)

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


;; Generates commands that writes a specific piece of text
;; Doesn't handle sticky properties, whatever that is
;; I'm using them for writing danish letters on
;; an english/american keyboard layout (I forget which)
;; in evil's insert state
(defun gurd-text-inserter (text prefix)
  () (dotimes (i (prefix-numeric-value prefix))
    (insert text)))

;; Switch to other buffer wtihout fanfare
;; Would be nice to go through the buffer list like tabs in
(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))


;;(global-set-key (kbd "C-<tab>") 'switch-to-other-buffer)
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

;; Makes the M-& and M-! shells run interactively so .bashrc is read
;; (setq shell-command-switch "-ic")

;;
;;: gurd
;;

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
(let ((mines-dir "~/.emacs.d/git/mines/"))
  (when (file-exists-p (concat mines-dir "mines.el"))
    (unless (file-exists-p (concat mines-dir "mines.elc"))
      (byte-compile-file (concat mines-dir "mines.el")))
    (load-file "~/.emacs.d/git/mines/mines.elc")))
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
;;[]

;;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Use-package
(eval-when-compile
  (require 'use-package))

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

;; Evil-mode
(use-package evil
  :ensure t
  :init
  (add-to-list 'load-path "~/.emacs.d/evil")
  :config
  (add-hook 'evil-mode-hook (lambda () (local-set-key (kbd "<tab>") 'indent-for-tab-command)))

  ;; Normal emacs indenting
  (evil-global-set-key 'normal (kbd "<tab>") 'indent-for-tab-command)
  (evil-global-set-key 'insert (kbd "<tab>") 'indent-for-tab-command)

  ;; Modes that should not start in evil
  (evil-set-initial-state 'tabulated-list-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'Info-mode 'emacs)
  (evil-set-initial-state 'image-dired-mode 'emacs)
  (evil-set-initial-state 'compilation-mode 'emacs)
  (evil-set-initial-state 'haskell-compilation-mode 'emacs)
  (evil-set-initial-state 'haskell-error-mode 'emacs)
  (evil-set-initial-state 'inferior-haskell-mode 'emacs)
  (evil-set-initial-state 'haskell-interactive-mode 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'Man-mode 'emacs)
  (evil-set-initial-state 'lisp-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'prolog-inferior-mode 'emacs)
  (evil-set-initial-state 'eww-mode 'emacs)
  (evil-set-initial-state 'gud-mode 'emacs)
  (evil-set-initial-state 'erc-mode 'emacs)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

  ;; (interactive "P")
  ;; Write the danish letters by s-(whatever their key would be)
  (evil-global-set-key 'insert (kbd "s-;")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?æ prefix)))
  (evil-global-set-key 'insert (kbd "s-:")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?Æ prefix)))
  (evil-global-set-key 'insert (kbd "s-'")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?ø prefix)))
  (evil-global-set-key 'insert (kbd "s-\"")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?Ø prefix)))
  (evil-global-set-key 'insert (kbd "s-[")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?å prefix)))
  (evil-global-set-key 'insert (kbd "s-{")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?Å prefix)))
  (evil-global-set-key 'emacs (kbd "s-;")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?æ prefix)))
  (evil-global-set-key 'emacs (kbd "s-:")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?Æ prefix)))
  (evil-global-set-key 'emacs (kbd "s-'")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?ø prefix)))
  (evil-global-set-key 'emacs (kbd "s-\"")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?Ø prefix)))
  (evil-global-set-key 'emacs (kbd "s-[")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?å prefix)))
  (evil-global-set-key 'emacs (kbd "s-{")
                       (lambda (prefix) (interactive "P")
                         (gurd-text-inserter ?Å prefix)))

  (evil-mode 1))


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


;; ido-mode auto paa
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-create-new-buffer 'always)
(defadvice ido-switch-buffer (around no-confirmation activate)
  (let ((confirm-nonexistent-file-or-buffer nil))
    ad-do-it))
(setq ido-auto-merge-work-directories-length -1)

;;Smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/mode-width 'full)
  (sml/setup)
  (sml/apply-theme 'dark)
  (put 'narrow-to-page 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (put 'upcase-region 'disabled nil))

;; ERC
;; (add-to-list 'load-path "~/.emacs.d/manual/erc")
;; (and
;;  (require 'erc-highlight-nicknames)
;;  (add-to-list 'erc-modules 'highlight-nicknames)
;;  (erc-update-modules))

;; (use-package erc
;;   :custom
;;   (erc-fill-static-center 22)

;;   :config
;;   (add-to-list 'load-path "~/.emacs.d/manual/erc")
;;   (and
;;      (require 'erc-highlight-nicknames)
;;      (add-to-list 'erc-modules 'highlight-nicknames)
;;      (erc-update-modules)))


;(use-package erc-highlight-nicknames)

;(use-package erc-hl-nicks
;  :after erc)

(use-package wiki-summary
  :ensure t
  :defer 1
  :bind ("C-c W" . wiki-summary))

(setq flycheck-standard-error-navigation nil)


(use-package evil-lispy
  :ensure t)

;;
;;:   LANGUAGES
;;

;; futhark
(use-package futhark-mode
  :ensure t
  :init
  (load-file "~/.emacs.d/git/fucheck/fucheck.el")
  (add-hook 'futhark-mode-hook 'fucheck-init)
  :bind
  ("C-c f"         . 'fucheck-next-test)
  ("C-c <tab>"     . 'fucheck-collapse-test)
  ("C-c C-c <tab>" . 'fucheck-collapse-all-tests)
  ("C-c t"         . 'fucheck-test-region)
  ("C-c C-c t"     . 'fucheck-test-all))



;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-tags-on-save t)
  (setq tags-revert-without-query t)
  (define-key haskell-mode-map (kbd "C-c j") 'haskell-mode-jump-to-def)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  ;; :hook
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

;; shell
(defvar sh-basic-offset 2)

;; Prolog
(use-package ediprolog
  :ensure t
  :config
  (global-set-key [f10] 'ediprolog-dwim)
  (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
  (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode)))

;;(require 'ediprolog)
;;(global-set-key [f10] 'ediprolog-dwim)
;;
;;(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;;(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; F#
;;(require 'fsharp-mode)
;;(add-hook 'fs-mode-hook #'smartparens-mode) ;Why this?

;; Emacs Lisp Elisp
(add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
(setq sentence-end-double-space nil)

;; Common Lisp
(let ((slime-file "~/quicklisp/slime-helper.el"))
  (when (file-exists-p slime-file)
    (load (expand-file-name "~/quicklisp/slime-helper.el"))))
(add-hook 'lisp-mode-hook #'evil-lispy-mode)

;; Replace "sbcl" with the path to your implementation
(defvar inferior-lisp-program "sbcl")

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
  :ensure t
  :config
  (add-hook 'arduino-mode-hook #'flycheck-arduino-setup))



;; Makes 'a' work in Dired
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'list-threads 'disabled nil)
(provide 'init)
;;; init.el ends here
