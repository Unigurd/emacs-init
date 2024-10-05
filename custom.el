;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (unless package-archive-contents (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("30dd125473a46e597d487e382c3e653973b2cb604a95d65aa81cceb66d446d9f" "5a71f7b4f84feaf6aeb80666af20da32f20f1c70fca1c5c6c6ea4ce5e988c176" "d15e68ecb65f0ce6796404a4c9c352c033501a8c9233354108a767858adfadf0" "0762f3ef374895fd5d7ff719de0823f96622d8240b8fb0a48bb0cbf451df7c1f" "410145e82e3c70c9769decb3e0e85f7c9f309991ae8c2ef11125e30ed661b7be" "5514f5f5ff0fcfb221d6694ce6ec6168c28144fa73769819d6267772e674debd" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(display-time-24hr-format t)
 '(doc-view-continuous t)
 '(erc-hl-nicks-mode t)
 '(erc-nick "Unigurd")
 '(evil-undo-system 'undo-tree)
 '(flycheck-emacs-lisp-load-path 'inherit)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(helm-external-programs-associations '(("pdf" . "okular")))
 '(org-agenda-files
   '("~/org/learn.org" "~/.emacs.d/elpa/org-ref-20200606.1848/org-ref.org" "~/org/uni.org" "~/org/adult.org" "~/org/mailCalendar.org"))
 '(package-selected-packages
   '(evil-paredit paredit ace-jump-mode font-lock-studio corfu blamer git-timemachine casual-symbol-overlay symbol-overlay casual-re-builder casual-isearch casual-ibuffer casual-info casual-dired embark-consult consult embark marginalia orderless vertico highlight-indent-guides highlight-symbol mines nix-mode persistent-scratch eros auctex helm flycheck-pkg-config idris-mode undo-tree geiser-guile geiser forth-mode flycheck-haskell slime evil-lispy haskell-process haskell-interactive-mode haskell-cabal wiki-summary haskell-mode arduino-mode flycheck cuda-mode lispy org-ref erc-hl-nicks use-package ace-window futhark-mode buffer-move ein pdf-tools transient magit evil dante intero ediprolog ## gnu-elpa-keyring-update oauth2 org-gcal calfw-org calfw cider rainbow-blocks rainbow-delimiters rainbow-mode markdown-mode projectile clojure-mode better-defaults smartparens w3m fsharp-mode))
 '(rainbow-delimiters-max-face-count 8)
 '(safe-local-variable-values '((flycheck-mode . t)))
 '(send-mail-function 'smtpmail-send-it)
 '(smtpmail-smtp-server "imap-mail.outlook.com")
 '(smtpmail-smtp-service 25)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil)))
 '(fringe ((t (:background "black"))))
 '(rainbow-delimiters-depth-1-face ((t (:inherit rainbow-delimiters-base-face :foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:inherit rainbow-delimiters-base-face :foreground "dodger blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:inherit rainbow-delimiters-base-face :foreground "orange"))))
 '(rainbow-delimiters-depth-4-face ((t (:inherit rainbow-delimiters-base-face :foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:inherit rainbow-delimiters-base-face :foreground "magenta"))))
 '(rainbow-delimiters-depth-6-face ((t (:inherit rainbow-delimiters-base-face :foreground "light sky blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:inherit rainbow-delimiters-base-face :foreground "peru"))))
 '(rainbow-delimiters-depth-8-face ((t (:inherit rainbow-delimiters-base-face :foreground "cyan"))))
 '(rainbow-delimiters-depth-9-face ((t (:inherit rainbow-delimiters-base-face :foreground "pale green"))))
 '(symbol-overlay-default-face ((t (:background "gray30" :inherit highlight)))))
(put 'narrow-to-region 'disabled nil)

