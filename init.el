;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(display-time-24hr-format t)
 '(doc-view-continuous t)
 '(erc-hl-nicks-mode t)
 '(erc-nick "Unigurd")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(package-selected-packages
   (quote
    (erc-hl-nicks use-package ace-window futhark-mode buffer-move ein pdf-tools transient magit evil dante intero ediprolog ## gnu-elpa-keyring-update oauth2 org-gcal calfw-org calfw cider rainbow-blocks rainbow-delimiters rainbow-mode markdown-mode projectile clojure-mode better-defaults smartparens w3m fsharp-mode)))
 '(rainbow-delimiters-max-face-count 8)
 '(send-mail-function (quote smtpmail-send-it))
 '(tramp-syntax (quote ftp)))
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
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "yellow"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "peru"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "white")))))
(put 'narrow-to-region 'disabled nil)


;;
;;:   SETUP
;;

;; currently only used for lexical-let somewhere later
;; Should it be moved?
(require 'cl)

;; Disable emacs startup-screen
(setq inhibit-startup-screen t)

;;Removes the bars
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)

;;Show line and column number in the mode line
(linum-mode 1)
(column-number-mode 1)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;;Stores backup files in ~/.emacs.d/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; displays the time and date in the mode line
;;(display-time)

(global-set-key (kbd "M-O") 'kill-this-buffer)

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
(global-set-key (kbd "M-2") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-3") 'delete-window)        ; close current pane
(global-set-key (kbd "M-4") 'split-window-right)   ; split pane left/right
(global-set-key (kbd "M-5") 'split-window-below)   ; split pane top/bottom

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
                        'term-mode) ; Should that be changed to special-mode?
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
(defun generate-interactive-text-inserter (text)
  (lexical-let ((text text))
    (lambda (arg)
      (interactive "P")
      (dotimes (i (prefix-numeric-value arg))
        (insert text)))))


;; Switch to other buffer wtihout fanfare
;; Would be nice to go through the buffer list like tabs in
(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))
(global-set-key (kbd "C-<tab>") 'switch-to-other-buffer)


;; Press f3 in shell-command to insert current file name
(define-key minibuffer-local-map [f3]
  (lambda() (interactive) (insert (buffer-file-name (nth 1 (buffer-list))))))

;;
;;:   PACKAGES
;;

;;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))


;; Use-package
(eval-when-compile
  (require 'use-package))


;; Sometimes Custom changes the value of tramp-syntax from this
;; in the preamble. Uncomment if appropriate.
;; (set-variable tramp-syntax "ftp")


;; ace-window
(use-package ace-window
  :config
  (bind-key (kbd "M-o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))



;; Evil-mode
(use-package evil
  :init
  (add-to-list 'load-path "~/.emacs.d/evil")
  :config
  (add-hook 'evil-mode-hook (lambda () (local-set-key (kbd "<tab>") 'indent-for-tab-command))))

;; Normal emacs indenting
(evil-global-set-key 'normal (kbd "<tab>") 'indent-for-tab-command)
(evil-global-set-key 'insert (kbd "<tab>") 'indent-for-tab-command)

;; Modes that should not start in evil
(add-to-list 'evil-emacs-state-modes 'tabulated-list-mode)
(add-to-list 'evil-emacs-state-modes 'dired-mode)
; Change help-mode like this because it already is in evil-motion-state-modes
(evil-set-initial-state 'help-mode 'emacs)

;; Write the danish letters by s-(whatever their key would be)
(evil-global-set-key 'insert (kbd "s-;")  (generate-interactive-text-inserter ?æ))
(evil-global-set-key 'insert (kbd "s-:")  (generate-interactive-text-inserter ?Æ))
(evil-global-set-key 'insert (kbd "s-'")  (generate-interactive-text-inserter ?ø))
(evil-global-set-key 'insert (kbd "s-\"") (generate-interactive-text-inserter ?Ø))
(evil-global-set-key 'insert (kbd "s-[")  (generate-interactive-text-inserter ?å))
(evil-global-set-key 'insert (kbd "s-{")  (generate-interactive-text-inserter ?Å))
(evil-global-set-key 'emacs  (kbd "s-;")  (generate-interactive-text-inserter ?æ))
(evil-global-set-key 'emacs  (kbd "s-:")  (generate-interactive-text-inserter ?Æ))
(evil-global-set-key 'emacs  (kbd "s-'")  (generate-interactive-text-inserter ?ø))
(evil-global-set-key 'emacs  (kbd "s-\"") (generate-interactive-text-inserter ?Ø))
(evil-global-set-key 'emacs  (kbd "s-[")  (generate-interactive-text-inserter ?å))
(evil-global-set-key 'emacs  (kbd "s-{")  (generate-interactive-text-inserter ?Å))
;(define-key evil-motion-state-minor-mode-map (kbd "<tab>") 'indent-for-tab-command)
(evil-mode 1)

;; rainbow
(use-package rainbow-blocks)
(use-package rainbow-delimiters
  :config
  ;; rainbow-delimiters automatisk paa
  (define-globalized-minor-mode my-global-rainbow-delimiters-mode rainbow-delimiters-mode
    (lambda () (rainbow-delimiters-mode 1)))
  (my-global-rainbow-delimiters-mode 1)
  )


;; org-mode
(use-package org
  :config
  (bind-key (kbd "C-c l") 'org-store-link)
  (bind-key (kbd "C-c a") 'org-agenda)
  (bind-key (kbd "C-c c") 'org-capture)
  ;; The files in my global org thing
  (setq org-agenda-files (list "~/org/uni.org"
                               "~/org/adult.org"
                               "~/org/mailCalendar.org")))


;; calendar
(use-package calfw)
(use-package calfw-org)

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


;;Smart mode line
(use-package smart-mode-line
  :config
  (setq sml/mode-width 'full)
  (sml/setup)
  (sml/apply-theme 'dark)
  (put 'narrow-to-page 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (put 'upcase-region 'disabled nil))

;; Spotify client
;; Downloaded from https://github.com/danielfm/spotify.el

(use-package spotify
  :init
  ;; Note that the original spotify-connect.el used if-let*.
  ;; I changed that because I couldn't find it
  ;; Not sure if they do the same, but it seems to work fine.
  (add-to-list 'load-path "~/.emacs.d/manual/spotify")

  :config
  (setq spotify-oauth2-client-id     "a5358da03e3841f3af5f5ff0c14750ea")
  (setq spotify-oauth2-client-secret "a10c46aa87bc4df5a73acf065740076d")
  (define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map)

  ;; Connect allows to control Spotify on other devices
  (setq spotify-transport 'connect)

  ;; Global spotify-remote-mode
  (define-globalized-minor-mode my-global-spotify-remote-mode spotify-remote-mode
    (lambda () (spotify-remote-mode 1)))
  )

;; ERC

(add-to-list 'load-path "~/.emacs.d/manual/erc")
(and
 (require 'erc-highlight-nicknames)
 (add-to-list 'erc-modules 'highlight-nicknames)
 (erc-update-modules))

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
  :defer 1
  :bind ("C-c W" . wiki-summary))

;;
;;:   LANGUAGES
;;

;; futhark
(use-package futhark-mode)

;; Haskell
(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)

(define-key haskell-mode-map (kbd "C-c j") 'haskell-mode-jump-to-def)
(define-key haskell-interactive-mode-map (kbd "C-`") 'haskell-interactive-bring)

;; PLD-LISP
(add-to-list 'auto-mode-alist '("\\.le\\'" . lisp-mode))

;; Prolog
(require 'ediprolog)
(global-set-key [f10] 'ediprolog-dwim)

(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; F#
;;(require 'fsharp-mode)
;;(add-hook 'fs-mode-hook #'smartparens-mode) ;Why this?

;; Common Lisp
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

;; C
;; use spaces instead of tabs, at least in cc mode
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2) ; Assuming you want your tabs to be four spaces wide
(defvaralias 'c-basic-offset 'tab-width)

; c indentation is now 2 spaces, i think
(setq c-default-style "bsd"
      c-basic-offset 2)

;; Java?
;;indents arguments to functions with long names better
(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)

;; Python
;; set python interpreter
(setq python-shell-interpreter "python3")








;;
;;:   E-MAIL
;;

;;(require 'org-mime)
;;(setq org-mime-library 'mml)
;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
;; (require 'mu4e)
;;   (setq mu4e-maildir (expand-file-name "~/mail/Outlook_Offlineimap"))

;;                                         ; get mail
;;    (setq mu4e-get-mail-command "mbsync -c ~/.emacs.d/mu4e/.mbsyncrc -a"
;;          ;; mu4e-html2text-command "w3m -T text/html" ;;using the default mu4e-shr2text
;;          mu4e-view-prefer-html t
;;          mu4e-update-interval 180
;;          mu4e-headers-auto-update t
;;          mu4e-compose-signature-auto-include nil
;;          mu4e-compose-format-flowed t)

;;    ;; to view selected message in the browser, no signin, just html mail
;;    (add-to-list 'mu4e-view-actions
;;                 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;;    ;; enable inline images
;;    (setq mu4e-view-show-images t)
;;    ;; use imagemagick, if available
;;    (when (fboundp 'imagemagick-register-types)
;;      (imagemagick-register-types))

;;    ;; every new email composition gets its own frame!
;;    (setq mu4e-compose-in-new-frame t)

;;    ;; don't save message to Sent Messages, IMAP takes care of this
;;    (setq mu4e-sent-messages-behavior 'delete)

;;    (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;;    ;; <tab> to navigate to links, <RET> to open them in browser
;;    (add-hook 'mu4e-view-mode-hook
;;              (lambda()
;;                ;; try to emulate some of the eww key-bindings
;;                (local-set-key (kbd "<RET>") 'mu4e~view-browse-url-from-binding)
;;                (local-set-key (kbd "<tab>") 'shr-next-link)
;;                (local-set-key (kbd "<backtab>") 'shr-previous-link)))

;;    ;; from https://www.reddit.com/r/emacs/comments/bfsck6/mu4e_for_dummies/elgoumx
;;    (add-hook 'mu4e-headers-mode-hook
;;              (defun my/mu4e-change-headers ()
;; 	           (interactive)
;; 	           (setq mu4e-headers-fields
;; 	                 `((:human-date . 25) ;; alternatively, use :date
;; 		               (:flags . 6)
;; 		               (:from . 22)
;; 		               (:thread-subject . ,(- (window-body-width) 70)) ;; alternatively, use :subject
;; 		               (:size . 7)))))

;;    ;; if you use date instead of human-date in the above, use this setting
;;    ;; give me ISO(ish) format date-time stamps in the header list
;;                                         ;(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")

;;    ;; spell check
;;    (add-hook 'mu4e-compose-mode-hook
;;              (defun my-do-compose-stuff ()
;;                "My settings for message composition."
;;                (visual-line-mode)
;;                (org-mu4e-compose-org-mode)
;;                (use-hard-newlines -1)
;;                (flyspell-mode)))

;;    (require 'smtpmail)

;;    ;;rename files when moving
;;    ;;NEEDED FOR MBSYNC
;;    (setq mu4e-change-filenames-when-moving t)

;;    ;;set up queue for offline email
;;    ;;use mu mkdir  ~/Maildir/acc/queue to set up first
;;    (setq smtpmail-queue-mail nil)  ;; start in normal mode

;;    ;;from the info manual
;;    (setq mu4e-attachment-dir  "~/Downloads")

;;    (setq message-kill-buffer-on-exit t)
;;    (setq mu4e-compose-dont-reply-to-self t)

;;    (require 'org-mu4e)

;;    ;; convert org mode to HTML automatically
;;    (setq org-mu4e-convert-to-html t)

;;    ;;from vxlabs config
;;    ;; show full addresses in view message (instead of just names)
;;    ;; toggle per name with M-RET
;;    (setq mu4e-view-show-addresses 't)

;;    ;; don't ask when quitting
;;    (setq mu4e-confirm-quit nil)

;;    ;; mu4e-context
;;    (setq mu4e-context-policy 'pick-first)
;;    (setq mu4e-compose-context-policy 'always-ask)
;;    (setq mu4e-contexts
;;          (list
;;           (make-mu4e-context
;;            :name "work" ;;for acc1-gmail
;;            :enter-func (lambda () (mu4e-message "Entering context work"))
;;            :leave-func (lambda () (mu4e-message "Leaving context work"))
;;            :match-func (lambda (msg)
;; 		                 (when msg
;; 		                   (mu4e-message-contact-field-matches
;; 		                    msg '(:from :to :cc :bcc) "acc1@gmail.com")))
;;            :vars '((user-mail-address . "acc1@gmail.com")
;; 	               (user-full-name . "User Account1")
;; 	               (mu4e-sent-folder . "/acc1-gmail/[acc1].Sent Mail")
;; 	               (mu4e-drafts-folder . "/acc1-gmail/[acc1].drafts")
;; 	               (mu4e-trash-folder . "/acc1-gmail/[acc1].Bin")
;; 	               (mu4e-compose-signature . (concat "Formal Signature\n" "Emacs 25, org-mode 9, mu4e 1.0\n"))
;; 	               (mu4e-compose-format-flowed . t)
;; 	               (smtpmail-queue-dir . "~/Maildir/acc1-gmail/queue/cur")
;; 	               (message-send-mail-function . smtpmail-send-it)
;; 	               (smtpmail-smtp-user . "acc1")
;; 	               (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
;; 	               (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
;; 	               (smtpmail-default-smtp-server . "smtp.gmail.com")
;; 	               (smtpmail-smtp-server . "smtp.gmail.com")
;; 	               (smtpmail-smtp-service . 587)
;; 	               (smtpmail-debug-info . t)
;; 	               (smtpmail-debug-verbose . t)
;; 	               (mu4e-maildir-shortcuts . ( ("/acc1-gmail/INBOX"            . ?i)
;; 					                           ("/acc1-gmail/[acc1].Sent Mail" . ?s)
;; 					                           ("/acc1-gmail/[acc1].Bin"       . ?t)
;; 					                           ("/acc1-gmail/[acc1].All Mail"  . ?a)
;; 					                           ("/acc1-gmail/[acc1].Starred"   . ?r)
;; 					                           ("/acc1-gmail/[acc1].drafts"    . ?d)
;; 					                           ))))
;;           (make-mu4e-context
;;            :name "personal" ;;for acc2-gmail
;;            :enter-func (lambda () (mu4e-message "Entering context personal"))
;;            :leave-func (lambda () (mu4e-message "Leaving context personal"))
;;            :match-func (lambda (msg)
;; 		                 (when msg
;; 		                   (mu4e-message-contact-field-matches
;; 		                    msg '(:from :to :cc :bcc) "acc2@gmail.com")))
;;            :vars '((user-mail-address . "acc2@gmail.com")
;; 	               (user-full-name . "User Account2")
;; 	               (mu4e-sent-folder . "/acc2-gmail/[acc2].Sent Mail")
;; 	               (mu4e-drafts-folder . "/acc2-gmail/[acc2].drafts")
;; 	               (mu4e-trash-folder . "/acc2-gmail/[acc2].Trash")
;; 	               (mu4e-compose-signature . (concat "Informal Signature\n" "Emacs is awesome!\n"))
;; 	               (mu4e-compose-format-flowed . t)
;; 	               (smtpmail-queue-dir . "~/Maildir/acc2-gmail/queue/cur")
;; 	               (message-send-mail-function . smtpmail-send-it)
;; 	               (smtpmail-smtp-user . "acc2")
;; 	               (smtpmail-starttls-credentials . (("smtp.gmail.com" 587 nil nil)))
;; 	               (smtpmail-auth-credentials . (expand-file-name "~/.authinfo.gpg"))
;; 	               (smtpmail-default-smtp-server . "smtp.gmail.com")
;; 	               (smtpmail-smtp-server . "smtp.gmail.com")
;; 	               (smtpmail-smtp-service . 587)
;; 	               (smtpmail-debug-info . t)
;; 	               (smtpmail-debug-verbose . t)
;; 	               (mu4e-maildir-shortcuts . ( ("/acc2-gmail/INBOX"            . ?i)
;; 					                           ("/acc2-gmail/[acc2].Sent Mail" . ?s)
;; 					                           ("/acc2-gmail/[acc2].Trash"     . ?t)
;; 					                           ("/acc2-gmail/[acc2].All Mail"  . ?a)
;; 					                           ("/acc2-gmail/[acc2].Starred"   . ?r)
;; 					                           ("/acc2-gmail/[acc2].drafts"    . ?d)
;; 					                           ))))))


;; Makes 'a' work in Dired
(put 'dired-find-alternate-file 'disabled nil)
