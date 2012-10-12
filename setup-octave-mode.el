(add-hook 'octave-mode-hook
	  (lambda ()
		  (abbrev-mode 1)
		  (auto-fill-mode 1)
		  (if (eq window-system 'x)
		      (font-lock-mode 1))
		  (octave-auto-indent 1)
		  (octave-auto-newline 1)))

(add-hook 'octave-mode-hook 'RET-behaves-as-LFD)

(provide 'setup-octave-mode)
