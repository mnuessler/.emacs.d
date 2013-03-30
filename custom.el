;; Custom settings

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "/usr/texbin/pdflatex -synctex=1")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-choose-browser))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(csv-align-style (quote auto))
 '(csv-separators (quote ("," ";" "|")))
 '(eproject-completing-read-function (quote eproject--ido-completing-read))
 '(erc-nick "mnuessler")
 '(erc-server "irc.iscout.local")
 '(erc-user-full-name "Matthias Nuessler")
 '(global-whitespace-mode t)
 '(inhibit-startup-screen t)
 '(ispell-program-name "/sw/bin/ispell")
 '(jira-url "https://jira")
 '(latex-run-command "/usr/texbin/pdflatex -synctex=1")
 '(octave-block-offset 4)
 '(org-agenda-files (quote ("~/Dropbox/org/private.org" "~/Dropbox/org/dev.org" "~/Dropbox/org/quotations.org")))
 '(php-completion-file "~/.emacs.d/auxiliary-files/php-mode/php-functions.txt")
 '(php-manual-path "~/opt/php-manual")
 '(php-mode-speedbar-open nil)
 '(py-fontify-shell-buffer-p t)
 '(py-shell-name "ipython")
 '(python-shell-interpreter "ipython")
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(sql-database "bobby")
 '(sql-oracle-program "sqlplus64")
 '(sql-product (quote postgres))
 '(sql-server "localhost")
 '(whitespace-style (quote (trailing space-before-tab empty))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:inherit highlight :background "#222"))))
 '(highlight ((t (:background "#222"))))
 '(hl-line ((t (:inherit highlight :background "#222")))))
