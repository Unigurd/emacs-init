(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(display-time-24hr-format t)
 '(package-selected-packages (quote (buffer-move w3m fsharp-mode)))
 '(tramp-syntax (quote default) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "black" :foreground "grey"))))
 '(fringe ((t (:background "black")))))
(put 'narrow-to-region 'disabled nil)



;;; Initialize MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(unless package-archive-contents (package-refresh-contents))
(package-initialize)

(require 'fsharp-mode)
(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)


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

;;Resize windows
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'enlarge-window)
;; (global-set-key (kbd "S-C-<up>") 'shrink-window)

;;C-S-D works as delete
(global-set-key (kbd "C-S-D") 'backward-delete-char-untabify)

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
