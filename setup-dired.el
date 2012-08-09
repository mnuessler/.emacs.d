;; Set to non-nil to enable recursive deletion of directories
(setq dired-recursive-deletes nil)

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

(provide 'setup-dired)
