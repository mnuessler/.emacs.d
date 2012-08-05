(require 'python-mode)

(setq ipython-command "/sw/bin/ipython")
(require 'ipython)

(require 'lambda-mode)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

(add-hook 'python-mode-hook #'lambda-mode 1)
(add-hook 'python-mode-hook #'highlight-indentation-on)
(add-hook 'python-mode-hook #'camelCase-mode 1)
(add-hook 'python-mode-hook #'fci-mode 1)

(provide 'setup-python-mode)

