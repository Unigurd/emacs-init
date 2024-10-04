;;; -*- lexical-binding: t -*-


(defun gurd-slime-inspector-link-finder (limit)
  (let ((match (text-property-search-forward 'slime-part-number)))
    (when (and match (> limit (prop-match-end match)))
      (set-match-data (list (prop-match-beginning match) (prop-match-end match))))
    match))

(font-lock-add-keywords
 'slime-inspector-mode
 `((,#'gurd-slime-inspector-link-finder . font-lock-builtin-face)))

(advice-add 'slime-sexp-at-point :filter-return
            (lambda (arg)
              "Prepend ' to read sexp if current-prefix-arg."
              ;; TODO: find way to check if some ancestor fun is called interactively
              (if current-prefix-arg
                  (concat "'" arg)
                arg)))


(provide 'gurd-slime)
