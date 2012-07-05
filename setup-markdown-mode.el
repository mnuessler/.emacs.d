;; Check balance of parantheses upon save in markdown mode
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (when buffer-file-name
	      (add-hook 'after-save-hook
			'check-parens
			nil t))))

(provide 'setup-markdown-mode)