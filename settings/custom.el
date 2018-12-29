;; Custom settings.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-choose-browser))
 '(confirm-kill-emacs (quote y-or-n-p))
 '(counsel-projectile-switch-project-action
   (quote
    (1
     ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
     ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
     ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
     ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
     ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
     ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
     ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
     ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
     ("C" counsel-projectile-switch-project-action-configure "run project configure command")
     ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
     ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
     ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
     ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
     ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
     ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
     ("O" counsel-projectile-switch-project-action-org-capture "org-capture into project"))))
 '(csv-align-style (quote auto))
 '(csv-separators (quote ("," ";" "|")))
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" default)))
 '(elfeed-goodies/entry-pane-position (quote bottom))
 '(elpy-rpc-python-command "python3")
 '(git-commit-setup-hook
   (quote
    (git-commit-save-message git-commit-setup-changelog-support git-commit-turn-on-auto-fill git-commit-turn-on-flyspell git-commit-propertize-diff with-editor-usage-message)))
 '(go-guess-gopath-functions
   (quote
    (go-dep-gopath go-gb-gopath go-godep-gopath go-wgo-gopath go-plain-gopath)))
 '(kubernetes-kubectl-executable "/home/matthias/google-cloud-sdk/bin/kubectl")
 '(neo-smart-open t)
 '(nxml-slash-auto-complete-flag t)
 '(package-selected-packages
   (quote
    (lsp-mode debbugs persp-projectile perspective magithub closql treepy drag-stuff drag-struff flycheck-swift eshell-bookmark intero elisp-docstring-mode string-edit elisp-def bash-completion shackle projectile-ripgrep google-translate sqlup-mode ob-elixir alchemist persistent-scratch magit-gerrit flycheck-yamllint diff-hl comment-tags dimmer dired-collapse el-get company-shell autodisass-java-bytecode ensime git-timemachine x509-mode docker-compose-mode ob-http ob-ipython ob-mongo ob-php ac-octave ob-go ob-prolog ob-redis ob-rust ob-sql-mode ssh-tunnels edbi dash-at-point realgud counsel-dash racer zeal-at-point go-rename indent-tools flymake-shell elfeed-org yapfify go-eldoc robe rvm flymake-ruby inf-ruby rubocop protobuf-mode 2048-game lua-mode auctex highlight-indentation-mode shm meghanada speed-type org org-projectile elfeed-goodies elfeed edit-server indium magit-tramp highlight-symbol gitattributes-mode gitignore-mode elpy flycheck-mypy jedi magit-gh-pulls ibuffer-projectile groovy-mode counsel-projectile esup ereader jira-markup-mode command-log-mode buffer-move goto-last-change dired-imenu idomenu go-guru flycheck-gometalinter go-autocomplete ag go-mode company-ansible company-restclient rainbow-delimiters smartparens zenburn-theme flycheck-ledger ledger-mode swift-mode cargo rust-mode gist prolog scala-mode toml-mode hackernews haskell-mode hacker-typer crontab-mode ssh-config-mode apache-mode nginx-mode xkcd csv-mode which-key ac-php markdown-mode plantuml-mode php-mode web-mode zencoding-mode js2-mode company cider terminal-here highlight-indentation yaml-mode ob-restclient restclient yasnippet dired+ ansible window-numbering bookmark+ back-button gitconfig-mode browse-kill-ring smooth-scrolling mu4e-maildirs-extension ace-flyspell guess-language slack nyan-mode all-the-icons all-the-icons-dired diminish exec-path-from-shell ace-jump-mode ace-mc docker docker-tramp dockerfile-mode emacs-xkcd embrace editorconfig neotree anti-zenburn-theme expand-region shell-pop undo-tree multiple-cursors ansible-doc ansible-vault projectile swiper smart-mode-line smart-mode-line-powerline-theme use-package kubernetes)))
 '(projectile-mode t nil (projectile))
 '(safe-local-variable-values
   (quote
    ((projectile-project-compilation-cmd . "./gradlew build -x check")
     (test-commaned . "./build.py './gradlew build -x dockerApacheProxyBuild -x dockerPostfixBuild'"))))
 '(save-place t nil (saveplace))
 '(shell-pop-universal-key "C-$")
 '(shell-pop-window-position "bottom")
 '(show-paren-mode t)
 '(sql-mysql-options (quote ("--protocol=tcp")))
 '(whitespace-style (quote (trailing space-before-tab empty)))
 '(yas-snippet-dirs
   (quote
    ("~/.emacs.d/elpa/yasnippet-0.13.0/snippets" "/home/matthias/.emacs.d/snippets" "/home/matthias/.emacs.d/elpa/ansible-20170926.1951/snippets"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
