(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-time-24hr-format t)
 '(package-selected-packages
   '(org-gcal calfw-org calfw cider rainbow-blocks rainbow-delimiters rainbow-mode markdown-mode projectile clojure-mode better-defaults pdf-tools ein smartparens buffer-move w3m fsharp-mode))
 '(rainbow-delimiters-max-face-count 8)
 '(tramp-syntax 'default nil (tramp)))
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



;;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(unless package-archive-contents (package-refresh-contents))
;;(package-initialize)

(require 'fsharp-mode)

(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


;;
;;    INDENTATION NAVIGATION COMMANDS
;;

(defun indentation-get-next-good-line (direction skip good)
  "Moving in direction `direction', and skipping over blank lines and lines that
satisfy relation `skip' between their indentation and the original indentation,
finds the first line whose indentation satisfies predicate `good'."
  (let ((starting-indentation (current-indentation))
        (lines-moved direction))
    (save-excursion
      (while (and (zerop (forward-line direction))
                  (or (eolp)  ; Skip past blank lines and other skip lines
                      (funcall skip (current-indentation) starting-indentation)))
        (setq lines-moved (+ lines-moved direction)))
      ;; Now we can't go further. Which case is it?
      (if (and
           (not (eobp))
           (not (bobp))
           (funcall good (current-indentation) starting-indentation))
          lines-moved
        nil))))

(defun indentation-get-next-sibling-line ()
  "The line number of the next sibling, if any."
  (indentation-get-next-good-line 1 '> '=))

(defun indentation-get-previous-sibling-line ()
  "The line number of the previous sibling, if any"
  (indentation-get-next-good-line -1 '> '=))

(defun indentation-get-parent-line ()
  "The line number of the parent, if any."
  (indentation-get-next-good-line -1 '>= '<))

(defun indentation-get-child-line ()
  "The line number of the first child, if any."
  (indentation-get-next-good-line +1 'ignore '>))


(defun indentation-move-to-line (func preserve-column name)
  "Move the number of lines given by func. If not possible, use `name' to say so."
  (let ((saved-column (current-column))
        (lines-to-move-by (funcall func)))
    (if lines-to-move-by
        (progn
          (forward-line lines-to-move-by)
          (move-to-column (if preserve-column
                              saved-column
                            (current-indentation))))
      (message "No %s to move to." name))))

(defun indentation-forward-to-next-sibling ()
  "Move to the next sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-next-sibling-line t "next sibling"))

(defun indentation-backward-to-previous-sibling ()
  "Move to the previous sibling if any, retaining column position."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-previous-sibling-line t "previous sibling"))

(defun indentation-up-to-parent ()
  "Move to the parent line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-parent-line nil "parent"))

(defun indentation-down-to-child ()
  "Move to the first child line if any."
  (interactive "@")
  (indentation-move-to-line 'indentation-get-child-line nil "child"))


;;Sets indentation navigation
(global-set-key (kbd "C-S-n")     'indentation-forward-to-next-sibling)
(global-set-key (kbd "C-S-p")     'indentation-backward-to-previous-sibling)
(global-set-key (kbd "C-S-b")     'indentation-up-to-parent)
(global-set-key (kbd "C-S-f")     'indentation-down-to-child)


;;
;; END OF INDENTATION COMMANDS
;;


;;  smartparens
(require 'smartparens-config)
(add-hook 'fs-mode-hook #'smartparens-mode)
(global-set-key (kbd "C-M-f")     'sp-forward-sexp)
(global-set-key (kbd "C-M-b")     'sp-backward-sexp)
(global-set-key (kbd "C-M-n")     'sp-down-sexp)
(global-set-key (kbd "C-M-p")     'sp-backward-up-sexp)

;; Disable emacs startup-screen
(setq inhibit-startup-screen t)

;;change default browser for 'browse-url'  to w3m
(setq browse-url-browser-function 'w3m-goto-url-new-session)

;;change w3m user-agent to android
(setq w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533.")

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
(disable-mouse-mode 1)

;;Move between windows with S-arrows
(windmove-default-keybindings)

;;Overwrite selected content
(delete-selection-mode)

;; move to the window in the other direction of C-x o
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))


;;highlighting matching parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; make ibuffer default buffer manager
(defalias 'list-buffers 'ibuffer) 
;;Resize windows
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'enlarge-window)
;; (global-set-key (kbd "S-C-<up>") 'shrink-window)

;;C-S-D works as delete
(global-set-key (kbd "C-S-D") 'backward-delete-char-untabify)

;;M-S-D deletes word backward
;; (defun backward-delete-word ()
;;   (let ((beg (point))) (backward-word 1) (delete-region beg (point))))
;; (global-set-key (kbd "M-D") 'backward-delete-word)

;;Removes the scrollbar and toolbar
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)


;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
;;Stores backup files in ~/.emacs.d/backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;Show line and column number in the mode line
(linum-mode 1)
(column-number-mode 1)
;; displays the time and date in the mode line
;;(setq calendar-date-display-form ((format "%s-%.2d-%.2d" year (string-to-number month) (string-to-number day)))
(display-time)
;; Displays battery percentage in the mode line
(display-battery-mode 1)

;;Smart mode line
;;(use-package smart-mode-line)
(setq sml/mode-width 'full)
(sml/setup)
(sml/apply-theme 'dark)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)


(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")



;; switch other window to default buffer
(global-set-key (kbd "C-x r")
		        '((switch-to-buffer)
		          (other-window)
		          (switch-to-buffer)))


;; multiple cursors
(require 'multiple-cursors)
;;cursor on each highlighted line
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;cursor on each highlighted thing
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; use spaces instead of tabs, at least in cc mode
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4) ; Assuming you want your tabs to be four spaces wide
(defvaralias 'c-basic-offset 'tab-width)

;;indents arguments to functions with long names better
(defun my-indent-setup ()
  (c-set-offset 'arglist-intro '+))
(add-hook 'java-mode-hook 'my-indent-setup)

;; set python interpreter
(setq python-shell-interpreter "python3")

;; Kan jeg ikke faa til at virke
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)
(global-set-key (kbd "<M-z>")     'zap-up-to-char)

;;Virker vist heller ikke
(require 'saveplace)
(setq-default save-place t)


;; rainbow-delimiters automatisk paa
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook  #'rainbow-delimiters-mode)
(add-hook 'latex-mode-hook #'rainbow-delimiters-mode)

(require 'rainbow-blocks)


                                        ; show trailing whitespace
(setq-default show-trailing-whitespace t)

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))
(global-set-key [(M C i)] 'aj-toggle-fold)

                                        ; c indentation is now 2 spaces, i think
(setq c-default-style "bsd"
      c-basic-offset 2)

                                        ; dired-jump always available
(global-set-key (kbd "C-x C-j") 'dired-jump)


;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
(global-set-key (kbd "C-c u") 'revert-buffer-no-confirm)


(defun open-delimiter-p ()
  "Is character at point a closing delimiter according to the
syntax table?"
  (eq ?\( (char-syntax (char-after))))

                                        ; goto mark
(defun goto-mark ()
  (interactive)
  "Jump to mark while setting mark to the old point"
  (let ((old-point (point)))
    (goto-char (mark))
    (pop-mark)
    (push-mark old-point)))
(global-set-key (kbd "C-M-g") 'goto-mark)

(defun substitute-parens (point mark opener closer)
  (cond
   ((open-delimiter-p)
    (mark-sexp)
    (goto-mark)
    (backward-delete-char-untabify 1)
    (insert closer)
    (goto-mark)
    (delete-forward-char 1)
    (insert opener)
    (goto-char point)
    (pop-mark))
   (message "Not at delimiter")))


(defun substitute-parens-input (point mark)
  (interactive "r")
  (cond
   ((open-delimiter-p)
    (let ((opener (read-string "Replace opening delimiter with: "))
          (closer (read-string "Replace closing delimiter with: ")))
      (substitute-parens point mark opener closer)))
   (t
    (message "Not at delimiter"))))


(defun delete-parens (point mark)
  (interactive "r")
  (substitute-parens point mark "" ""))


(global-set-key (kbd "C-M-<") 'substitute-parens-input)
(global-set-key (kbd "C-M-<backspace>") 'delete-parens)


;; Setup Agda-mode
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))


(require 'org-mime)
(setq org-mime-library 'mml)
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
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
(put 'dired-find-alternate-file 'disabled nil)

;; Stuff for activating org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; The files in my global org thing
(setq org-agenda-files (list "~/org/uni.org"
                             "~/org/adult.org"))

;; calendar
(require 'calfw)
(require 'calfw-org)

;; google calendar sync

(require 'org-gcal)
