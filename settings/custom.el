;; Custom settings.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(browse-url-browser-function 'browse-url-choose-browser)
 '(confirm-kill-emacs 'y-or-n-p)
 '(counsel-projectile-switch-project-action
   '(4
     ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
     ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
     ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
     ("D" counsel-projectile-switch-project-action-dired "open project in dired")
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
     ("si" counsel-projectile-switch-project-action-git-grep "search project with git grep")
     ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
     ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
     ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
     ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
     ("Oc" counsel-projectile-switch-project-action-org-capture "capture into project")
     ("Oa" counsel-projectile-switch-project-action-org-agenda "open project agenda")))
 '(csv-align-style 'auto)
 '(csv-separators '("," ";" "|"))
 '(custom-safe-themes
   '("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" default))
 '(dired-sidebar-subtree-line-prefix "__")
 '(ediff-show-ancestor t)
 '(ediff-split-window-function 'split-window-horizontally t)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(elfeed-goodies/entry-pane-position 'bottom)
 '(elpy-rpc-python-command "python3")
 '(frog-jump-buffer-posframe-parameters '(:internal-border-width 5))
 '(git-commit-setup-hook
   '(git-commit-save-message git-commit-setup-changelog-support git-commit-turn-on-auto-fill git-commit-turn-on-flyspell git-commit-propertize-diff with-editor-usage-message))
 '(go-guess-gopath-functions
   '(go-dep-gopath go-gb-gopath go-godep-gopath go-wgo-gopath go-plain-gopath))
 '(gofmt-command "gofmt")
 '(ledger-reports
   '(("Expenses food this month" "ledger --effective --sort date --period \"this month\" reg \"Expenses:Food\" --real --price EUR")
     (#("BVG monthly expenses" 0 1
        (idx 1))
      "ledger --effective --sort date --monthly reg \"Public Transport\" and @BVG --real")
     (#("Work Lunch Monthly" 0 1
        (idx 2))
      "ledger --effective --sort date --monthly reg Expenses:Food:Work")
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
      "%(binary) -f %(ledger-file) --effective --sort date reg %(account)")))
 '(ledger-schedule-file "~/Documents/ledger/ledger-schedule.ledger")
 '(ledger-schedule-look-forward 90)
 '(lsp-clients-go-server "gopls")
 '(lsp-ui-doc-delay 0.8)
 '(lsp-ui-doc-max-width 75)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(neo-smart-open t)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files
   '("~/Dropbox/org/private.org" "~/Dropbox/org/dev.org" "~/Dropbox/org/quotations.org" "~/Dropbox/org/todo.org"))
 '(org-modules
   '(org-bbdb org-bibtex org-docview org-eww org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m))
 '(package-selected-packages
   '(bazel-mode uuidgen all-the-icons-ivy-rich ivy-rich yaml-imenu ox-reveal htmlize edbi flymake-shell flymake-ruby auctex haskell-mode ereader jira-markup-mode flycheck-swift gist ac-octave prolog-mode hacker-typer crontab-mode apache-mode ac-php dired-x dired ace-flyspell auto-package-update goto-last-change beacon csv-mode vterm x509-mode flycheck-mypy adoc-mode md4rd dired+ writeroom-mode olivetti flycheck-rust eglot company-tern frog-jump-buffer aggressive-fill-paragraph avy-zap dired-sidebar direnv epresent markdown-mode meghanada minions mu4e-maildirs-extension multiple-cursors mutt-mode neotree nginx-mode no-littering nyan-mode ob-elixir ob-go ob-http ob-ipython ob-mongo ob-prolog ob-redis ob-restclient ob-rust ob-sql-mode ob-swift org org-bookmark-heading org-present org-presie org-projectile org-tree-slide persistent-scratch persp-mode persp-mode-projectile-bridge php-mode plantuml-mode powerline presentation projectile prolog protobuf-mode racer rainbow-delimiters realgud robe rubocop rust-mode rvm sbt-mode scala-mode shell-pop shm slack smart-mode-line smart-mode-line-powerline-theme smartparens smartparens-mode smooth-scrolling speed-type ssh-config-mode ssh-tunnels string-edit swift-mode terminal-here terraform-mode toml-mode treemacs treemacs-magit undo-tree use-package visible-mark web-mode which-key window-numbering xkcd yaml-mode yapfify yasnippet yasnippet-snippets zeal-at-point zenburn-theme zencoding-mode))
 '(plantuml-server-url "https://www.plantuml.com/pte")
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".elixir-ls"))
 '(racer-rust-src-path
   "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")
 '(safe-local-variable-values
   '((projectile-project-compilation-cmd . "go build ./...")
     (projectile-project-compilation-cmd . "go build ./..")
     (projectile-globally-ignored-directories . ".elixir_ls")
     (test-commaned . "./build.py './gradlew build -x dockerApacheProxyBuild -x dockerPostfixBuild'")))
 '(save-place t nil (saveplace))
 '(shell-pop-shell-type '("vterm" "*shell*" (lambda nil (shell))))
 '(shell-pop-universal-key "C-$")
 '(shell-pop-window-position "bottom")
 '(show-paren-mode t)
 '(sql-mysql-options '("--protocol=tcp"))
 '(terraform-indent-level 2)
 '(whitespace-style '(trailing space-before-tab empty))
 '(yas-snippet-dirs
   '("~/.emacs.d/elpa/yasnippet-0.13.0/snippets" "/home/matthias/.emacs.d/snippets" "/home/matthias/.emacs.d/elpa/ansible-20170926.1951/snippets")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
