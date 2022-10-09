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
 '(evil-undo-system 'undo-tree)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(helm-external-programs-associations '(("pdf" . "okular")))
 '(org-agenda-files
   '("~/org/learn.org" "~/.emacs.d/elpa/org-ref-20200606.1848/org-ref.org" "~/org/uni.org" "~/org/adult.org" "~/org/mailCalendar.org"))
 '(package-selected-packages
   '(persistent-scratch eros auctex helm flycheck-pkg-config idris-mode undo-tree geiser-guile geiser forth-mode flycheck-haskell slime evil-lispy haskell-process haskell-interactive-mode haskell-cabal wiki-summary haskell-mode arduino-mode flycheck cuda-mode lispy org-ref erc-hl-nicks use-package ace-window futhark-mode buffer-move ein pdf-tools transient magit evil dante intero ediprolog ## gnu-elpa-keyring-update oauth2 org-gcal calfw-org calfw cider rainbow-blocks rainbow-delimiters rainbow-mode markdown-mode projectile clojure-mode better-defaults smartparens w3m fsharp-mode))
 '(rainbow-delimiters-max-face-count 8)
 '(safe-local-variable-values '((flycheck-mode . t)))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "imap-mail.outlook.com")
 '(smtpmail-smtp-service 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 92 :width normal :foundry "ADBO" :family "Source Code Pro"))))
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

