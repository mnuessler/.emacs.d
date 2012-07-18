(autoload 'camelCase-mode "camelCase-mode" nil t)
(add-hook 'java-mode-hook '(lambda () (camelCase-mode 1)))

(provide 'setup-java-mode)

