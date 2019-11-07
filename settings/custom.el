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
 '(ediff-show-ancestor nil)
 '(ediff-split-window-function (quote split-window-horizontally) t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(elfeed-goodies/entry-pane-position (quote bottom))
 '(elpy-rpc-python-command "python3")
 '(git-commit-setup-hook
   (quote
    (git-commit-save-message git-commit-setup-changelog-support git-commit-turn-on-auto-fill git-commit-turn-on-flyspell git-commit-propertize-diff with-editor-usage-message)))
 '(go-guess-gopath-functions
   (quote
    (go-dep-gopath go-gb-gopath go-godep-gopath go-wgo-gopath go-plain-gopath)))
 '(gofmt-command "gofmt")
 '(kubernetes-kubectl-executable "/home/matthias/google-cloud-sdk/bin/kubectl")
 '(ledger-reports
   (quote
    (("Expenses food this month" "ledger -f /Users/matthiasnuessler/Documents/ledger/my2.ledger --effective --sort date --period \"this month\" reg \"Expenses:Food\" --real --price EUR")
     (#("BVG monthly expenses" 0 1
        (idx 1))
      "ledger -f /Users/matthiasnuessler/Documents/ledger/my2.ledger --effective --sort date --monthly reg \"Public Transport\" and @BVG --real")
     (#("Work Lunch Monthly" 0 1
        (idx 2))
      "ledger -f /Users/matthiasnuessler/Documents/ledger/my2.ledger --effective --sort date --monthly reg Expenses:Food:Work")
     (#("bal" 0 1
        (idx 3))
      "%(binary) -f %(ledger-file) bal")
     (#("reg" 0 1
        (idx 4))
      "%(binary) -f %(ledger-file) --effective --sort date reg")
     (#("payee" 0 1
        (idx 5))
      "%(binary) -f %(ledger-file) reg @%(payee)")
     (#("account" 0 1
        (idx 6))
      "%(binary) -f %(ledger-file) --effective --sort date reg %(account)"))))
 '(ledger-schedule-file "~/Documents/ledger/ledger-schedule.ledger")
 '(ledger-schedule-look-forward 90)
 '(lsp-clients-go-server "gopls")
 '(lsp-ui-doc-delay 0.8)
 '(lsp-ui-doc-max-width 75)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(neo-smart-open t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files
   (quote
    ("~/Dropbox/org/private.org" "~/Dropbox/org/dev.org" "~/Dropbox/org/quotations.org" "~/Dropbox/org/todo.org")))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-eww org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m)))
 '(package-selected-packages
   (quote
    (2048-game
     ac-octave
     ac-php
     ace-flyspell
     ace-jump-mode
     ace-mc
     achievements
     ag
     alchemist
     all-the-icons
     all-the-icons-dired
     all-the-icons-ivy
     ansible
     ansible-doc
     ansible-vault
     apache-mode
     auctex
     autodisass-java-bytecode
     back-button
     bookmark+
     browse-kill-ring
     buffer-move
     cargo
     cider
     clj-refactoring
     clojure-cheatsheet
     command-log-mode
     company
     company-ansible
     company-restclient
     company-sourcekit
     counsel-dash
     counsel-projectile
     crontab-mode
     csv-mode
     dash-at-point
     diminish
     dimmer
     dired+
     dired-collapse
     dired-imenu
     docker
     docker-compose-mode
     docker-tramp
     dockerfile-mode
     edbi
     edit-server
     editorconfig
     eldoc-overlay
     elfeed
     elfeed-goodies
     elfeed-org
     elisp-docstring-mode
     elpy
     emacs-xkcd
     embrace
     ensime
     ereader
     esup
     exec-path-from-shell
     expand-region
     flycheck-golangci-lint
     flycheck-gometalinter
     flycheck-ledger
     flycheck-mypy
     flymake-go
     flymake-ruby
     flymake-shell
     ggtags
     gist
     git-gutter
     git-timemachine
     gitattributes-mode
     gitconfig-mode
     gitignore-mode
     go-autocomplete
     go-eldoc
     go-guru
     go-mode
     go-rename
     goto-last-change
     groovy-mode
     guess-language
     hacker-typer
     hackernews
     haskell-mode
     highlight-indentation
     highlight-indentation-mode
     highlight-symbol
     htmlize
     ibuffer-projectile
     idomenu
     indent-tools
     indium
     inf-ruby
     intero
     jedi
     jira-markup-mode
     js2-mode
     kubernetes
     ledger-mode
     lsp-go
     lua-mode
     magit-gerrit
     magit-gh-pulls
     magit-tramp
     magithub
     markdown-mode
     meghanada
     minions
     mu4e-maildirs-extension
     multiple-cursors
     neotree
     nginx-mode
     no-littering
     nyan-mode
     ob-elixir
     ob-go
     ob-http
     ob-ipython
     ob-mongo
     ob-php
     ob-prolog
     ob-redis
     ob-restclient
     ob-rust
     ob-sql-mode
     org
     org-present
     org-presie
     org-projectile
     ox-reveal
     persp-mode
     persp-mode-projectile-bridge
     php-mode
     plantuml-mode
     popup-imenu
     projectile
     projectile-ripgrep
     prolog
     protobuf-mode
     racer
     rainbow-delimiters
     realgud
     restclient
     robe
     rubocop
     rust-mode
     rvm
     scala-mode
     shell-pop
     shm
     slack
     smart-mode-line
     smart-mode-line-powerline-theme
     smartparens
     smooth-scrolling
     speed-type
     ssh-config-mode
     ssh-tunnels
     string-edit
     swift-mode
     swiper
     terminal-here
     toml-mode
     undo-tree
     unfill
     use-package
     web-mode
     web-mode-edit-element
     which-key
     window-numbering
     x509-mode
     xkcd
     yaml-mode
     yapfify
     yasnippet
     yasnippet-snippets
     zeal-at-point
     zenburn-theme
     zencoding-mode)))
 '(racer-rust-src-path
   "~/.rustup/toolchains/nightly-x86_64-apple-darwin/lib/rustlib/src/rust/src")
 '(safe-local-variable-values
   (quote
    ((test-commaned . "./build.py './gradlew build -x dockerApacheProxyBuild -x dockerPostfixBuild'"))))
 '(save-place t nil (saveplace))
 '(shell-pop-universal-key "C-$")
 '(shell-pop-window-position "bottom")
 '(show-paren-mode t)
 '(sql-mysql-options (quote ("--protocol=tcp")))
 '(terraform-indent-level 2)
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
