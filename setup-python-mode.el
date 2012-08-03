(require 'lambda-mode)

(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))
(add-hook 'python-mode-hook #'lambda-mode 1)
(add-hook 'python-mode-hook #'highlight-indentation-on)

(provide 'setup-python-mode)
