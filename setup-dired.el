;; Set to non-nil to enable recursive deletion of directories
(setq dired-recursive-deletes t)

;; move files or directories into the operating system's Trash, instead of deleting them outright
(setq delete-by-moving-to-trash t)

;; let search commands limit themselves to the file names (C-s behaves like M-s f C-s),
;; but only when point was on a file name initially
(setq dired-isearch-filenames 'dwim)

;; Set up dired-x
(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    ;; Set dired-x global variables here.  For example:
	    ;; (setq dired-guess-shell-gnutar "gtar")
	    ;; (setq dired-x-hands-off-my-keys nil)
	    ))
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.
	    (dired-omit-mode 1)
	    ))

;; don't create new buffer each time moving up a directory
;; source: http://www.emacswiki.org/emacs-es/DiredReuseDirectoryBuffer
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file "..")))
	    ))

;; dired-sync provides a simple and easy way to synchronize directories
(add-hook 'dired-mode-hook
	  (lambda ()
	    (when (require 'dired-sync nil t)
	      (define-key dired-mode-map (kbd "C-c S") 'dired-do-sync))))

(provide 'setup-dired)
