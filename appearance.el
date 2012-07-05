;; Adjust frame size
(if (window-system)
    (set-frame-size (selected-frame) 100 60))

;; Display line number and column number
(setq line-number-mode t)
(setq column-number-mode t)

;; Use bleeding edge org-mode cloned instead of bundled version
(setq load-path (cons "~/.emacs.d/org-mode/lisp" load-path))
(setq load-path (cons "~/.emacs.d/org-mode/contrib/lisp" load-path))
(require 'org-install)

(provide 'appearance)
