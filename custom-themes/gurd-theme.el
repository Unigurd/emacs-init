(deftheme gurd
  "Created 2024-05-17.")

(custom-theme-set-faces
 'gurd
 '(fringe ((t (:background "black"))))
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil)))
 '(mode-line-inactive ((t nil)))
 '(mode-line ((t :foreground "gray60" :background "black" :inverse-video nil)))
 '(sml/global ((t :foreground "gray50" :inverse-video nil)))
 '(sml/modes ((t :inherit sml/global :foreground "White")))
 '(sml/filename ((t :inherit sml/global :foreground "#eab700" :weight bold)))
 '(sml/prefix ((t :inherit sml/global :foreground "#bf6000")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
 '(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
 '(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename)))
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "PfEd" :family "Liberation Mono"))))
 '(mode-line-active ((t (:inherit mode-line :background "gray15")))))

(provide-theme 'gurd)
