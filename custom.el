;; Custom settings

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(LaTeX-command "/usr/texbin/pdflatex -synctex=1")
 '(browse-url-browser-function (quote browse-url-choose-browser))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(csv-separators (quote (",")))
 '(eproject-completing-read-function (quote eproject--ido-completing-read))
 '(erc-nick "mnuessler")
 '(erc-server "irc.iscout.local")
 '(erc-user-full-name "Matthias Nuessler")
 '(global-whitespace-mode t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/sw/bin/ispell")
 '(jira-url "https://jira")
 '(latex-run-command "/usr/texbin/pdflatex -synctex=1")
 '(php-completion-file "~/.emacs.d/auxiliary-files/php-mode/php-functions.txt")
 '(php-manual-path "~/opt/php-manual")
 '(php-mode-speedbar-open nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(sql-oracle-program "sqlplus64")
 '(whitespace-style (quote (trailing space-before-tab empty))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:inherit highlight :background "#222"))))
 '(highlight ((t (:background "#222"))))
 '(hl-line ((t (:inherit highlight :background "#222")))))
