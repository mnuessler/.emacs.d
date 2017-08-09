;; Custom settings.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-choose-browser))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(csv-align-style (quote auto))
 '(csv-separators (quote ("," ";" "|")))
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" default)))
 '(elfeed-goodies/entry-pane-position (quote bottom))
 '(elpy-rpc-python-command "python3")
 '(kubernetes-kubectl-executable "/home/matthias/google-cloud-sdk/bin/kubectl")
 '(package-selected-packages
   (quote
    (docker-compose-mode ob-http ob-ipython ob-mongo ob-php ac-octave ob-go ob-prolog ob-redis ob-rust ob-sql-mode ssh-tunnels edbi dash-at-point realgud counsel-dash racer zeal-at-point go-rename indent-tools flymake-shell elfeed-org yapfify go-eldoc robe rvm flymake-ruby inf-ruby rubocop protobuf-mode 2048-game lua-mode auctex highlight-indentation-mode shm meghanada speed-type org org-projectile elfeed-goodies elfeed edit-server indium magit-tramp magithub highlight-symbol gitattributes-mode gitignore-mode elpy flycheck-mypy jedi magit-gh-pulls ibuffer-projectile groovy-mode counsel-projectile esup ereader jira-markup-mode command-log-mode buffer-move goto-last-change magit-gerrit dired-imenu idomenu go-guru flycheck-gometalinter go-autocomplete ag go-mode company-ansible company-restclient clojure-cheatsheet rainbow-delimiters smartparens zenburn-theme flycheck-ledger ledger-mode swift-mode cargo rust-mode gist persp-mode persp-mode-projectile-bridge prolog scala-mode toml-mode hackernews haskell-mode hacker-typer crontab-mode ssh-config-mode apache-mode nginx-mode xkcd csv-mode which-key ac-php markdown-mode plantuml-mode php-mode web-mode zencoding-mode js2-mode company cider terminal-here highlight-indentation yaml-mode ob-restclient restclient yasnippet dired+ ansible window-numbering bookmark+ back-button gitconfig-mode browse-kill-ring smooth-scrolling mu4e-maildirs-extension ace-flyspell guess-language slack nyan-mode all-the-icons all-the-icons-dired diminish exec-path-from-shell ace-jump-mode ace-mc docker docker-tramp dockerfile-mode emacs-xkcd embrace editorconfig neotree anti-zenburn-theme expand-region shell-pop undo-tree multiple-cursors ansible-doc ansible-vault projectile swiper smart-mode-line smart-mode-line-powerline-theme use-package kubernetes)))
 '(projectile-mode t nil (projectile))
 '(safe-local-variable-values
   (quote
    ((test-commaned . "./build.py './gradlew build -x dockerApacheProxyBuild -x dockerPostfixBuild'"))))
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(whitespace-style (quote (trailing space-before-tab empty))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
