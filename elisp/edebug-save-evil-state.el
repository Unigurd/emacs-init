;;; -*- lexical-binding: t -*-


(require 'edebug)
(require 'evil)

(defvar gurd-edebug-saved-evil-state nil
  "Used by `gurd-edebug-save-restore-evil-state' to save the evil state to restore.")
(make-local-variable 'gurd-edebug-saved-evil-state)

(defun gurd-edebug-save-restore-evil-state ()
  "Save & restore evil state when entering & exiting `edebug-mode'.
Meant to be added to `edebug-mode-hook'"
  (cond (edebug-mode
         (setf gurd-edebug-saved-evil-state evil-state)
         (evil-change-state 'emacs))
        (gurd-edebug-saved-evil-state
         (evil-change-state gurd-edebug-saved-evil-state)
         (setf gurd-edebug-saved-evil-state nil))))

(provide 'edebug-save-evil-state)
