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

;; (use-package find-func
;;   :config
;;   ;; Find emacs C source directory
;;   (let* ((version (progn
;;                     (string-match "GNU Emacs \\([0-9]+\\.[0-9]+\\)" (version))
;;                     (match-string 1 (version))))
;;          (c-source-dir (concat "~/.emacs.d/c-source/emacs-" version "/src")))
;;     (when (file-exists-p c-source-dir)
;;       (setq find-function-C-source-directory c-source-dir))))


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


(use-package gurd-commands
  :demand
  :after (vertico)
  :bind (("C-S-x k" . gurd-kill-other-buffer)
         :map lisp-data-mode-map
         ("C-c *" . earmuffs))
  :config
  (with-no-warnings  (advice-add #'vertico-insert :after #'gurd-minibuffer-normalize-path)))


(use-package minibuffer
  :after (gurd-commands)
  :bind
  (:map minibuffer-local-map
        ("M-<backspace>" . gurd-updir)))


;; higher contrast (of what?)
(use-package shr-color
  :defer t
  :config
  (setf shr-color-visible-luminance-min 70))

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

(use-package hl-line
  :config
  (global-hl-line-mode))


;; This used to be:
;; (use-package latex
;;   :ensure auctex)
;; But I couldn't find a `latex' package with C-h P,
;; I don't know why I don't just (use-package auctex)
;; And I want to get rid of `:ensure's. But I'm keeping
;; this note in case something has broken next time I'm
;; writing latex
(use-package  auctex)

(use-package symbol-overlay
  :bind-keymap ("C-c g" . symbol-overlay-map)
  :bind (:map symbol-overlay-map
              ("M-n" . symbol-overlay-switch-forward)
              ("M-p" . symbol-overlay-switch-backward))
  :hook (after-change-major-mode . symbol-overlay-mode)
  :config
  (setf symbol-overlay-idle-time 0.2)
  ;; TODO: Highlight TODOs in programming modes
  )

(use-package casual-symbol-overlay
  :bind (("C-c G" . casual-symbol-overlay-tmenu)
         :map symbol-overlay-map
         ("?" . casual-symbol-overlay-tmenu))
  :after (symbol-overlay))

(use-package highlight-indent-guides
  :config
  (setf highlight-indent-guides-method 'character
        highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "dim gray")
  :hook
  (prog-mode . highlight-indent-guides-mode))

(use-package hideshow
  :hook
  (prog-mode . hs-minor-mode))



(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  ;; :custom
  ;; Suggestions from https://github.com/minad/vertico?tab=readme-ov-file#configuration
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;;
;;: gurd
;;
(use-package orderless
  :custom
  ;; Customized according to https://github.com/oantolin/orderless?tab=readme-ov-file#overview
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  ;; Also add to `completion-list-mode-map'?
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  ;; Commented since the displayed embark documentation seemed to always
  ;; just be "Embark on symbol X" for some X under point.
  ;; :init
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; Show all eldoc options instead of just the first, I think.
  ;; Not chosen because I think it might make the minibuffer jump up and down
  ;; depending on the number of shown docstrings
  ;; (setf eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  )

;; TODO: look into `cape'
(use-package corfu
  :custom
  (corfu-auto t)
  ;; Prevent corfu from interfering with mct (which I don't use as of this writing)
  ;; vertico (which I do), and password input.
  (global-corfu-minibuffer (lambda ()
                             (not (or (bound-and-true-p mct--active)
                                      (bound-and-true-p vertico--input)
                                      (eq (current-local-map) read-passwd-map)))))
  :init
  (global-corfu-mode)
  :config
  (keymap-unset corfu-map "RET" t)
  :bind (:map corfu-map
              ("TAB" . corfu-expand)      ; Expand common prefix
              ("M-TAB" . corfu-complete)) ; Use first suggestion
  ;; TODO: Make TAB work in evil-insert
  )

(use-package face-remap
  :bind ())

;;
;;:   PACKAGES
;;

(use-package emacs
  ;; What's the difference between :init and :config for the emacs package?
  :custom
  (enable-recursive-minibuffers t)
  ;; Don't include commands marked as applicable for a non-active mode.
  ;; Read Elisp info 22.2.4: Specifying Modes for Commands
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Do not allow the cursor in the minibuffer prompt as per
  ;; https://github.com/minad/vertico?tab=readme-ov-file#configuration
  (setf (plist-get minibuffer-prompt-properties 'cursor-intangible) t)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package wdired)

(use-package casual-dired
  :bind (:map dired-mode-map
              ;; TODO: Change to command that runs `casual-dired-tmenu' by
              ;; default and `dired-summary' with prefix?
              ("?" . casual-dired-tmenu)
              ("s" . casual-dired-sort-by-tmenu)
              ("/" . casual-dired-search-replace-tmenu)))

(use-package info
  :bind (:map Info-mode-map
              ;; Make info behave like in casual-info
              ("M-[" . Info-history-back)
              ("M-]" . Info-history-forward)
              ("h" . Info-prev)
              ("j" . Info-next-reference)
              ("k" . Info-prev-reference)
              ("l" . Info-next)
              ("/" . Info-search)
              ("B" . bookmark-set)))

(use-package casual-info
  :bind (:map Info-mode-map
              ;; Make normal info behave like casual-info
              ("?" . casual-info-tmenu)
              ("p" . casual-info-browse-backward-paragraph)
              ("n" . casual-info-browse-forward-paragraph)))

(use-package ibuffer
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :defer t
  :bind (:map ibuffer-mode-map
              ("{" . ibuffer-backwards-next-marked)
              ("}" . ibuffer-forward-next-marked)
              ("[" . ibuffer-backward-filter-group)
              ("]" . ibuffer-forward-filter-group)
              ("$" . ibuffer-toggle-filter-group)))

(use-package casual-ibuffer
  :bind (:map ibuffer-mode-map
              ("?" . casual-ibuffer-tmenu)
              ("F" . casual-ibuffer-filter-tmenu)
              ("s" . casual-ibuffer-sortby-tmenu)))

(use-package casual-isearch
  :bind (:map isearch-mode-map
              ("C-o" . casual-isearch-tmenu)))

(use-package casual-re-builder
  :bind (:map
         reb-mode-map ("C-o" . casual-re-builder-tmenu)
         :map
         reb-lisp-mode-map ("C-o" . casual-re-builder-tmenu))
  :after (re-builder))

(use-package flycheck
  :init
  ;; Autoloaded, so does calling it in :init cause `flycheck' to
  ;; get loaded while before `use-package' would have loaded it?
  (global-flycheck-mode)
  :config
  (setq flycheck-standard-error-navigation nil)
  (setq-default flycheck-disabled-checkers
                (cons 'emacs-lisp-checkdoc (default-value 'flycheck-disabled-checkers))))

(use-package magit)

(use-package git-timemachine)


;; TODO: Look into vundo, undo-fu
(use-package undo-tree
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
  :config
  (bind-key (kbd "C-M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; rainbow
(use-package rainbow-delimiters
  :hook (after-change-major-mode . (lambda ()
                                     (unless (and (eq major-mode 'help-mode)
                                                  (string= (buffer-name) "*Faces*"))
                                       (rainbow-delimiters-mode)))))

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


(use-package scratch-file-mode
  :hook (lisp-interaction-mode . scratch-file-mode))

(defun half-screen-scroll-advice (wrapped-fun &rest args)
  "Use this as :around advice for functions that move the screen a page down.
Makes them only move half a page down.
"
  (let ((next-screen-context-lines (/ (window-total-height) 2)))
    (apply wrapped-fun args)))

(advice-add 'evil-scroll-page-down :around #'half-screen-scroll-advice)
(advice-add 'evil-scroll-page-up :around #'half-screen-scroll-advice)
(advice-add 'scroll-up-command :around #'half-screen-scroll-advice)
(advice-add 'scroll-down-command :around #'half-screen-scroll-advice)
(advice-add 'Info-scroll-up :around #'half-screen-scroll-advice)
(advice-add 'Info-scroll-down :around #'half-screen-scroll-advice)



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


(use-package help-mode
  ;; Don't interpret ; as comment in help-mode
  :hook (help-mode . (lambda () (modify-syntax-entry 59 "."))))

;; My own highlight-help-parameters mode
(use-package highlight-help-parameters
  :hook (help-mode . highlight-help-parameters-mode))





;; Common Lisp
(use-package slime
  :after gurd-commands
  :config
  (let ((slime-file "~/quicklisp/slime-helper.el"))
    (when (file-exists-p slime-file)
      (load (expand-file-name "~/quicklisp/slime-helper.el"))))

  ;; Replace "sbcl" with the path to your implementation
  (setf inferior-lisp-program "sbcl")
  :bind
  (:map lisp-mode-map ("C-c *" . earmuffs)))




;; scheme
(use-package geiser
  :config
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

  (put 'with-sizes 'scheme-indent-function 2))


(use-package geiser-guile
  :after geiser)

(use-package geiser-mode
  ;; Just here to silence a warning in `gurd-geiser's `use-package'
  :after geiser)

(use-package gurd-geiser
  :after (geiser geiser-mode)
  :bind (:map geiser-mode-map
              ("C-c t" . gurd-geiser-test-dwim)
              ("C-c e" . gurd-geiser--send-minibuffer)
              ("C-c r" . gurd-geiser-restart-from-file)))



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
