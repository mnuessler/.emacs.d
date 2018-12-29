;; First the package management.
(package-initialize)

(when (>= emacs-major-version 24)
  (setq package-archives '(("elpa"         . "https://elpa.gnu.org/packages/")
                           ("marmalade"    . "https://marmalade-repo.org/packages/")
                           ("melpa"        . "https://melpa.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("elpy"         . "https://jorgenschaefer.github.io/packages/")
                           ("org"          . "https://orgmode.org/elpa/"))
        package-archive-priorities '(("elpy"         . 99)
                                     ("org"          . 42)
                                     ("melpa-stable" . 15)
                                     ("marmalade"    . 10)
                                     ("elpa"         . 5)
                                     ("melpa"        . 0))))

;; Check if we're on Emacs 24.4 or newer, if so, use the pinned package feature
;(when (boundp 'package-pinned-packages)
;  (setq package-pinned-packages
;       '((magit-gerrit . "melpa"))))

;(defun install-required-packages ()
;  (interactive)
;  (when (>= emacs-major-version 24)
;    (package-refresh-contents)
;    (mapc (lambda (package)
;            (unless (require package nil t)
;              (package-install package)))
;          package-selected-packages)))

;; Determine path to ".emacs.d" directory.
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Keep Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" (expand-file-name "settings" dotfiles-dir)))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))
;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Newline at end of file
(setq require-final-newline t)

;; Revert buffers automatically when underlying files are changed
;; externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Set up load path
(add-to-list 'load-path (expand-file-name "settings" dotfiles-dir))
(add-to-list 'load-path (expand-file-name "el-get" dotfiles-dir))
(if (file-directory-p "~/projects/mine/ob-influxdb")
    (add-to-list 'load-path "~/projects/mine/ob-influxdb"))

(require 'appearance)

;; Side-by-side diffs with ediff
(setq ediff-split-window-function 'split-window-horizontally)

(use-package smooth-scrolling
  :ensure t
  :defer 0
  :config
  (smooth-scrolling-mode 1))

(use-package browse-kill-ring
  :ensure t
  :config
  ;; Make M-y to use browse-kill-ring.
  (browse-kill-ring-default-keybindings))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package bookmark+)

;; (use-package back-button
;;   :config
;;   (back-button-mode 1))

(use-package window-numbering
  :config
  (window-numbering-mode 1))

(use-package recentf
  :ensure t
  :defer 0
  :config
  (setq recentf-save-file (expand-file-name "recentf" dotfiles-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; Disable recentf-cleanup on Emacs start, because it can
        ;; cause problems with remote files.
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; Get side-by-side diffs
(setq ediff-split-window-function 'split-window-horizontally)

;; Modes for various git configuration files.
;; https://github.com/magit/git-modes
(use-package gitconfig-mode
  :ensure t
  :defer t)
(use-package gitignore-mode
  :ensure t
  :defer t)
(use-package gitattributes-mode
  :ensure t
  :defer t)

;; Magit, a git porcelain inside Emacs.
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package magit-gerrit
  :disabled
  :ensure t
  :after magit
  :pin melpa)

;; Magit interfaces for GitHub
;; https://github.com/vermiculus/magithub
(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)
  (setq magithub-clone-default-directory "~/projects"))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-mc
  :bind (("C-)"   . ace-mc-add-multiple-cursors)
         ("C-M-)" . ace-mc-add-single-cursor)))

;; Make Emacs use the $PATH set up by the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "GOROOT")
  (exec-path-from-shell-copy-env "JAVA_HOME")
  (exec-path-from-shell-copy-env "PYTHONPATH")
  (exec-path-from-shell-copy-env "WORKON_HOME")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

(use-package edit-server
  :if window-system
  :init
;;  (add-hook 'after-init-hook '(lambda ()
;;                                (unless (server-running-p)
;;                                  (server-start))) t)
  (add-hook 'after-init-hook 'edit-server-start t)
  (setq edit-server-new-frame nil)
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode))))

;; Required by neotree icon theme and all-the-icons-dired-mode.
;; Requires installation of fonts to work correctly:
;; https://github.com/domtronn/all-the-icons.el/tree/master/fonts
;;
;; If icons are not displayed correctly, manually apply fix from:
;; https://github.com/domtronn/all-the-icons.el/pull/106
(use-package all-the-icons
  :defer t
  :after dired
  :diminish all-the-icons-dired-mode
  :init
  ;; Use icons in dired mode.
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Neotree. Available themes:
;; - classic (default)
;; - ascii
;; - arrow
;; - icons
;; - nerd
;; TODO make it open in current project dir or dir of current file
;; https://emacs.stackexchange.com/questions/29499/how-to-toggle-neotree-in-the-project-directory
(use-package neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'nerd 'ascii))
  :bind ([f8] . neotree-toggle))

(use-package ag
  :init
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
  (setq ag-reuse-window t))

;; Nyanyanyanyanyanyanya!
(use-package nyan-mode
  :disabled
  :if window-system)

;; GitHub: https://github.com/abo-abo/swiper
;; Doc: http://oremacs.com/swiper/
(use-package swiper
  :init
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers 0)
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ([f6]  . ivy-resume))
  :diminish ivy-mode)

;; https://github.com/abo-abo/swiper#counsel
(use-package counsel
  :ensure t
  :init
  (counsel-mode 1)
  :diminish counsel-mode)

;; Useful to read the email signature from a file.
;; Source: https://groups.google.com/d/msg/mu-discuss/kkhTgTgvlhc/eLiU8d0VMnsJ
(defun file-string (file)
  "Read the contents of a file and return as a string."
  (with-current-buffer (find-file-noselect file)
    (buffer-string)))

;; apt-get install offlineimap
;; apt-get install html2text
;; Cheat sheet:
;; http://www.djcbsoftware.nl/code/mu/mu4e/Keybindings.html#Keybindings
(use-package mu4e
  :load-path "/usr/share/emacs24/site-lisp/mu4e"
  :bind ("C-x M" . mu4e)
  :init
  ;; folders
  (setq mu4e-maildir "/home/matthias/Maildir"
        mu4e-drafts-folder "/[Gmail].Drafts"
        mu4e-sent-folder   "/[Gmail].Sent Mail"
        mu4e-trash-folder  "/[Gmail].Trash")
  ;; don't automatically mark messages as read
  (setq mu4e-view-auto-mark-as-read nil)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick, if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  ;; make mu4e the default program for composing mails
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Don't save message to Sent Messages, Gmail/IMAP takes care of this.
  (setq mu4e-sent-messages-behavior 'delete)
  ;; Don't show duplicate messages in search results.
  (setq mu4e-headers-skip-duplicates t)
  ;; Including related messages in search results.
  (setq mu4e-headers-include-related t)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  ;; setup some handy shortcuts
  ;; you can quickly switch to your Inbox -- press ``ji''
  ;; then, when you want archive some messages, move them to
  ;; the 'All Mail' folder by pressing ``ma''.

  (setq mu4e-maildir-shortcuts
        '(("/INBOX"             . ?i)
          ("/[Gmail].Sent Mail" . ?s)
          ("/[Gmail].Trash"     . ?t)
          ("/[Gmail].All Mail"  . ?a)))

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "offlineimap")

  ;; define email signature, but don't include it automatically
  (setq mu4e-compose-signature-auto-include nil
        mu4e-compose-signature (file-string "/home/matthias/.signature"))

  ;; Sending mail.
  (setq message-send-mail-function   'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server         "smtp.gmail.com"
        smtpmail-stream-type         'starttls
        smtpmail-smtp-service        587
        smtpmail-local-domain        "egym.de"
        smtpmail-queue-mail          nil)

  ;; something about ourselves
  (setq user-full-name    "Matthias Nüßler"
        user-mail-address (concat "matthias.nuessler@" smtpmail-local-domain))
  :config
  (add-hook 'mu4e-compose-mode-hook
            (defun my-do-compose-stuff ()
              "My settings for message composition."
              (set-fill-column 72)
              (flyspell-mode)
              (save-excursion
                (message-add-header
                 (concat "X-Mailer: mu4e/" mu4e-mu-version "\n")))))

  ;; (add-to-list 'mu4e-bookmarks
  ;;           (make-mu4e-bookmark
  ;;            :name  "Big messages"
  ;;            :query "size:5M..500M"
  ;;            :key ?b))
  (use-package mu4e-maildirs-extension)
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (mu4e-maildirs-extension))

  ;;(require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
;;  (defun gnus-dired-mail-buffers ()
;;    "Return a list of active message buffers."
;;    (let (buffers)
;;      (save-current-buffer
;;      (dolist (buffer (buffer-list t))
;;        (set-buffer buffer)
;;        (when (and (derived-mode-p 'message-mode)
;;                   (null message-sent-message-via))
;;          (push (buffer-name buffer) buffers))))
;;      (nreverse buffers)))

;;  (setq gnus-dired-mail-mode 'mu4e-user-agent)
;;  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))


;; http://www.djcbsoftware.nl/code/mu/mu4e/Other-search-functionality.html#Including-related-messages


;; Automatically detect language for Flyspell.
;; https://github.com/tmalsburg/guess-language.el
;; Show list of all dictionaries available for spell-checking:
;;   (mapcar 'car ispell-dictionary-alist)
(use-package guess-language
  :ensure t
  :defer t
  :init
  (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en" "English"))
                                   (de . ("german8" "German")))
        guess-language-languages '(en de)
        guess-language-min-paragraph-length 35)
  :diminish guess-language-mode)

(use-package ace-flyspell
  :defer t)

;; Ansible
;;
;; ansible-mode (https://github.com/k1LoW/emacs-ansible)
;; Ansible Vault support
;(global-set-key (kbd "C-c b") 'ansible::decrypt-buffer)
;(global-set-key (kbd "C-c g") 'ansible::encrypt-buffer)
;;
;; ansible-doc-mode (https://github.com/lunaryorn/ansible-doc.el)
(use-package ansible
  :ensure t
  :pin melpa
  :init
  (setq ansible::vault-password-file "~/.ansible-vault-pass")
  :config
  (add-hook 'ansible-hook #'ansible::auto-decrypt-encrypt)
  (add-hook 'ansible-hook #'ansible-doc-mode))

(require  'key-bindings)


(use-package tramp)
;;  :init
;;  (tramp-set-completion-function "ssh"
;;                                 '((tramp-parse-sconfig "~/.ssh/config"))))

(use-package dired
  :init
  ;; Set to non-nil to enable recursive deletion of directories
  (setq dired-recursive-deletes t)
  ;; Move files or directories into the operating system's Trash,
  ;; instead of deleting them outright
  (setq delete-by-moving-to-trash t)
  ;; Let search commands limit themselves to the file names (C-s
  ;; behaves like M-s f C-s), but only when point was on a file name
  ;; initially.
  (setq dired-isearch-filenames 'dwim)
  ;; Set up dired-x
  (add-hook 'dired-load-hook
            (lambda ()
              (load "dired-x")
              ;; Set dired-x global variables here.  For example:
              ;; (setq dired-guess-shell-gnutar "gtar")
              ;; (setq dired-x-hands-off-my-keys nil)
              ))
  (add-hook 'dired-mode-hook
            (lambda ()
              ;; Set dired-x buffer-local variables here.
              (dired-omit-mode 1)
              ))
  ;; Reuse dired buffer moving up a directory instead of creating a
  ;; new one every time.
  ;; Source: http://www.emacswiki.org/emacs-es/DiredReuseDirectoryBuffer
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "^")
                (lambda ()
                  (interactive)
                  (find-alternate-file "..")))))
  ;; Reuse dired buffer when entering subdirectory (requires dired+).
  ;; Source: http://www.emacswiki.org/emacs-es/DiredReuseDirectoryBuffer
  ;; (add-hook 'dired-mode-hook
  ;;           (lambda ()
  ;;             (when (require 'dired+ nil t)
  ;;               (progn
  ;;                 ;; hide-details-mode is bound to "(" in a dired buffer
  ;;                 (dired-hide-details-mode 0)
  ;;                 (diredp-toggle-find-file-reuse-dir 1)))))
  ;; dired-sync provides a simple and easy way to synchronize directories
  (add-hook 'dired-mode-hook
            (lambda ()
              (when (require 'dired-sync nil t)
                (define-key dired-mode-map (kbd "C-c S") 'dired-do-sync))))
  ;; Open file at point with gnome-open or mac open by pressing 'E'.
  (add-hook 'dired-mode-hook
            (lambda ()
              (local-set-key "E" 'dired-gnome-or-mac-open-file))))

(use-package dired-x
  :after dired)

(use-package dired-imenu
  :after dired)

;; Version in repo is outdated. Latest version can be downloaded from:
;; https://www.emacswiki.org/emacs/dired%2b.el
;; (Disabled because it messes up the color scheme.)
(use-package dired+
  :disabled
  :after dired
  (diredp-toggle-find-file-reuse-dir 1))

;; https://github.com/Fuco1/dired-hacks#dired-collapse
(use-package dired-collapse
  :ensure t
  :after dired
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              (dired-collapse-mode 1))))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode)

(use-package restclient
  :defer t
  :ensure t)

;; Major mode for PlantUML diagram definitions.
;; http://plantuml.com/
(use-package plantuml-mode
  :mode ("\\.plu\\'" "\\.plantuml\\'")
  :bind (:map plantuml-mode-keymap
              ("C-<f8>" . plantuml-complete-symbol)))

;; TODO mode mappings
(require 'setup-org-mode)

(use-package highlight-indentation
  :defer t
  :ensure t
  :diminish highlight-indentation-mode)

(use-package yaml-mode
  :init
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode))

;; Open _external_ terminal in current directory or project.
;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :bind (("C-<f5>" . terminal-here-launch)
         ("C-<f6>" . terminal-here-project-launch)))

;; Rainbow delimiters.
(use-package rainbow-delimiters
  :defer t)


;; Clojure
(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook
            (lambda ()
              (prettify-symbols-mode 1))))

;; CIDER: Clojure Interactive Development Environment for Emacs
;; https://github.com/clojure-emacs/cider
;; https://cider.readthedocs.io/en/latest/
(use-package cider
  :init
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook      #'company-mode)
  ;; Fuzzy matching
  ;(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;(add-hook 'cider-mode-hook      #'cider-company-enable-fuzzy-completion)
  ;; Enable CamelCase support for editing commands (like forward-word,
  ;; backward-word, etc), since we often have to deal with Java class
  ;; and method names.
  (add-hook 'cider-repl-mode-hook #'subword-mode)
  ;; Enable SmartParens.
  (add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
  ;; Show imenu menubar.
  (add-hook 'cider-mode-hook #'imenu-add-menubar-index)
  ;; Rainbow delimiters.
  (add-hook 'cider-mode-hook      #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; JavaScript
;; Deps: apt-get install nodejs
;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "nodejs")

(use-package zencoding-mode
  :config
  ;; Auto-start on any markup modes.
  (add-hook 'sgml-mode-hook 'zencoding-mode))

;; ac-php
;; https://github.com/xcwen/ac-php#install
;; Install php-cli command for phpctags:
;;   apt-get install php-cli
;; install cscope command for ac-php-cscope-find-egrep-pattern:
;;   apt-get install cscope
;; In project root:
;;   touch .ac-php-conf.json
(use-package php-mode
  :mode (("\\.php\'" . php-mode)
         ("\\.inc\'" . php-mode))
  :init
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2)
  (setq php-template-compatibility nil)
  (setq ac-sources '(ac-source-php))
  :config
  (use-package auto-complete)
  (use-package ac-php)
  (auto-complete-mode 1)
  ;; Enable eldoc
  (ac-php-core-eldoc-setup)
  :bind (:map php-mode-map
              ("C-]" . ac-php-find-symbol-at-point)
              ("C-t" . ac-php-location-stack-back)))

(use-package web-mode
  :mode (("\\.phtml\\'"     . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'"   . web-mode)
         ("\\.as[cp]x\\'"   . web-mode)
         ("\\.erb\\'"       . web-mode)
         ("\\.mustache\\'"  . web-mode)
         ("\\.djhtml\\'"    . web-mode)))

;; Source: https://github.com/jrblevin/markdown-mode
;; To use multimarkdown: apt-get install libtext-markup-perl perl-doc
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  :config
  ;; Check balance of parantheses upon save.
  (add-hook 'markdown-mode-hook
            (lambda ()
              (when buffer-file-name
                (add-hook 'after-save-hook 'check-parens nil t))))
  (add-hook 'markdown-mode-hook 'flyspell-prog-mode))
;; Display available keybindings in popup.
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :init
  ;; Set the time delay (in seconds) for the which-key popup to appear.
  (setq which-key-idle-delay 2.5)
  :config
  (which-key-mode)
  :diminish which-key-mode)

(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'")

;; View xkcd from Emacs, yay!
(use-package xkcd)

;; Make script files executable automatically.
;; Source: http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Terminal.
(use-package term
  :bind ([f2] . visit-ansi-term))

;; Start Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(use-package apache-mode
  :mode (("\\.htaccess\\'"                   . apache-mode)
         ("httpd\\.conf\\'"                  . apache-mode)
         ("srm\\.conf\\'"                    . apache-mode)
         ("access\\.conf\\'"                 . apache-mode)
         ("sites-\\(available\\|enabled\\)/" . apache-mode)))

;; Mode for editing Nginx config files.
;; Automatically enabled for:
;;   1) Files named `nginx.conf`
;;   2) Files ending in `.conf` under `nginx` directory
;; https://github.com/ajc/nginx-mode
(use-package nginx-mode)

;; https://github.com/jhgorrell/ssh-config-mode-el
(use-package ssh-config-mode
  :mode (("/\\.ssh/config\\'"     . ssh-config-mode)
         ("/sshd?_config\\'"      . ssh-config-mode)
         ("/known_hosts\\'"       . ssh-known-hosts-mode)
         ("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  :config
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock))

;; Mode for editing crontab files.
(use-package crontab-mode
  :mode (("\\.crontab\\'" . crontab-mode)
         ("/cron\\.d/"    . crontab-mode)))

;; Type like a hacker!
(use-package hacker-typer
  :defer t)

(use-package hackernews
  :defer t)

;; https://github.com/dryman/toml-mode.el
(use-package toml-mode
  :mode "\\.toml\\'")

;; Mode for editing Scala files.
;; https://github.com/ensime/emacs-scala-mode
(use-package scala-mode
  :defer t)

;; Prolog
(use-package prolog-mode
  :commands (run-prolog prolog-mode mercury-mode)
  :config
  (setq prolog-system 'sicstus))

;; GNU Octave (octave-mode is part of Emacs).
;; apt-get install octave
;; https://www.gnu.org/software/octave/doc/v4.0.0/Using-Octave-Mode.html
(add-hook 'octave-mode-hook
          (lambda ()
            (progn
              (abbrev-mode 1)
              (auto-fill-mode 1)
              (if (eq window-system 'x)
                  (font-lock-mode 1))
              (octave-auto-indent 1)
              (octave-auto-newline 1)
              (RET-behaves-as-LFD))))

;; https://github.com/coldnew/ac-octave
(use-package ac-octave
  :defer t
  :init
  (add-hook 'octave-mode-hook
            (lambda ()
              (progn
                (setq ac-sources '(ac-complete-octave))
                (auto-complete-mode 1)))))

;; EditorConfig support for Emacs.
;; http://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
;; apt-get install editorconfig
(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

;; Emacs integration for gist.github.com.
;; https://github.com/defunkt/gist.el
;; Go to your GitHub Settings and generate a personal access token with gist scope
;; Next run:
;;   git config --global github.user <your-github-user-name>
;;   git config --global github.oauth-token <your-personal-access-token-with-gist-scope>
(use-package gist
  :defer t)

;; http://orgmode.org/worg/org-tutorials/org-taskjuggler.html

;; Rust programming language.
;; https://github.com/rust-lang/rust-mode
;; https://github.com/kwrooijen/cargo.el/blob/master/cargo.el
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (use-package cargo)
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  ;; https://github.com/racer-rust/racer
  ;; Installation:
  ;;   $ cargo install racer
  ;; Configuration:
  ;;   $ rustup component add rust-src
  ;;   $ export RUST_SRC_PATH="$(rustc --print sysroot)/lib/rustlib/src/rust/src"
  (use-package racer
    :bind (:map rust-mode-map
                ("TAB" . company-indent-or-complete-common))
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    (add-hook 'racer-mode-hook #'company-mode)
    (setq company-tooltip-align-annotations t))
  (setq rust-format-on-save t))


;; Swift.
;; https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  :mode "\\.swift\\'")

;; https://github.com/swift-emacs/flycheck-swift
(use-package flycheck-swift
  :after (flycheck swift-mode)
  :init
  (eval-after-load 'flycheck '(flycheck-swift-setup)))

;; Completion for Swift projects via SourceKit(ten)
;; OSX only. Only works when there is a *.xcodeproj up the directory tree.
;; https://github.com/nathankot/company-sourcekit
(use-package company-sourcekit
  :if (memq window-system '(mac ns))
  :after company
  :init
  (add-to-list 'company-backends 'company-sourcekit))

;; Ledger
;; http://ledger-cli.org/3.0/doc/ledger-mode.html
;; https://github.com/purcell/flycheck-ledger
(use-package ledger-mode
  :mode "\\.ledger\\'"
  :config
  (add-hook 'ledger-mode-hook
            (lambda ()
              (progn
                (flycheck-mode 1)
                (yas-minor-mode 0))))
  (yas-minor-mode 0)
  (use-package flycheck-ledger))

;; Define keyboard macro for Euro unicode symbol.
(fset 'euro
      (lambda (&optional arg)
        "Keyboard macro."
        (interactive "p")
        (kmacro-exec-ring-item
         (quote ([24 56 return 35 120 50 48 65 67 return] 0 "%d")) arg)))

;; Golang
;; https://github.com/dominikh/go-mode.el
;; auto-complete-mode
;; flycheck-mode
;;
;;
;; go get -u github.com/nsf/gocode
;; go get golang.org/x/tools/cmd/guru
;;
;; Go guru - integration of the Go 'guru' analysis tool
;; https://github.com/dominikh/go-mode.el/blob/master/go-guru.el
(use-package go-mode
  ;; To jump back after using godef-jump.
  :bind ("M-*" . pop-tag-mark)
  :init
  (add-hook 'before-save-hook #'gofmt-before-save)
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook
            (lambda ()
              (progn
                (flycheck-mode 1)
                (go-eldoc-setup)
                (go-guru-hl-identifier-mode)
                (rainbow-delimiters-mode-enable)
                (local-set-key (kbd "M-.") #'godef-jump)
                (local-set-key (kbd "M-*") #'pop-tag-mark)
                (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
                (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
                (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
                (local-set-key (kbd "M-[") 'previous-error)
                (if (not (string-match "go" compile-command))
                    (set (make-local-variable 'compile-command)
                         "go build -v && go test -v && go vet")))))
  :config
  ;; https://github.com/alecthomas/gometalinter
  ;; go get -u github.com/alecthomas/gometalinter
  ;; gometalinter --install
  (use-package flycheck-gometalinter)
  (use-package auto-complete)
  (use-package go-autocomplete)
  (use-package go-guru)
  ;; Integration of the 'gorename' tool into Emacs.
  ;; https://github.com/dominikh/go-mode.el/blob/master/go-rename.el
  ;; % go get golang.org/x/tools/cmd/gorename
  ;; % go build golang.org/x/tools/cmd/gorename
  ;; % mv gorename $HOME/bin/         # or elsewhere on $PATH
  (use-package go-rename))
  ;;
  ;; https://github.com/syohex/emacs-go-eldoc
  ;; Dependencies:
  ;; - gocode: go get -u github.com/nsf/gocode
;;  (use-package go-eldoc
;;    :init
;;    (add-hook 'go-mode-hook #'go-eldoc-setup)))

;(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

; Move point through buffer-undo-list positions.
; https://github.com/camdez/goto-last-change.el
(use-package goto-last-change
  :commands (goto-last-change)
  :bind ("C-x C-\\" . goto-last-change))

; Swap buffers without typing C-x b on each window.
(use-package buffer-move
  :bind (("C-S-<up>"    . buf-move-up)
         ("C-S-<down>"  . buf-move-down)
         ("C-S-<left>"  . buf-move-left)
         ("C-S-<right>" . buf-move-right)))

; Imenu tag selection a la ido.
(use-package idomenu
  :ensure t
  :bind ("C-x C-i" . idomenu))

;; JSON
;; (json.el is part of GNU Emacs since 23.1)
(add-hook 'json-mode-hook #'flycheck-mode)
(add-hook 'json-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'after-save-hook 'check-parens nil t))))

;; Emacs-Lisp mode.
;; Check for unbalanced parenthesis after save.
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when buffer-file-name
              (add-hook 'after-save-hook 'check-parens nil t))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; Use spaces, not tabs.
            (setq indent-tabs-mode nil)
            ;; Pretty-print eval'd expressions.
            (define-key emacs-lisp-mode-map
              "\C-x\C-e" 'pp-eval-last-sexp)
            ;; Recompile if .elc exists.
            ;;(add-hook (make-local-variable 'after-save-hook)
            ;;          (lambda ()
            ;;            (byte-force-recompile default-directory)))
            ;; Indent line after pressing ENTER.
            (define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)))
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; Turn on 'flyspell-mode' for comments and strings.
;; Requires Ispell: apt-get install ispell
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
;; Enable Smart-Parens mode.
(add-hook 'emacs-lisp-mode-hook 'turn-on-smartparens-mode)
;; Show imenu menubar.
(add-hook 'emacs-lisp-mode-hook 'imenu-add-menubar-index)
;; Allow quick jump to package declarations within init.el
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (when (equal "~/.emacs.d/init.el" buffer-file-truename)
              (setq imenu-generic-expression '((nil "(use-package \\(.*\\)" 1))))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (progn
              (company-mode 1)
              (auto-complete-mode 0))))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (prettify-symbols-mode 1)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode 1)))


;; JIRA/Confluence markup
;; https://github.com/mnuessler/jira-markup-mode
(use-package jira-markup-mode
  :commands (jira-markup-mode)
  :mode (("\\.confluence\\'"                    . jira-markup-mode)
         ("/itsalltext/.*jira.*\\.txt\\'"       . jira-markup-mode)
         ("/itsalltext/.*confluence.*\\.txt\\'" . jira-markup-mode))
  :config
  (add-hook 'jira-markup-mode-hook
            (lambda ()
              (word-wrap t))))

;; Epub reader for emacs with org-mode integration.
;; https://github.com/bddean/emacs-ereader
(use-package ereader :defer t)

;; Use hippie-expand instead of dabbrev.
;; Hippie expand is dabbrev expand on steroids.
(use-package hippie-exp
  :bind ("\M- " . hippie-expand)
  :init
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(global-set-key "\M- " #'hippie-expand)

(global-set-key "\M-*" #'pop-tag-mark)

;; Perspectives for Emacs.
;; https://github.com/nex3/perspective-el
(use-package perspective
  :disabled
  :defer t
  :config
  (persp-mode 1))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              ("s-p"   . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :init
  ;; The default action is projectile-find-file. Switch to project
  ;; root directory instead.
  (setq projectile-switch-project-action #'projectile-dired)
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (go-set-project)))
  :config
  (counsel-projectile-mode))

;; Projectile integration for perspective.el.
;; https://github.com/bbatsov/persp-projectile
(use-package persp-projectile
  :defer t
  :after (perspective projectile))

;; Counsel-projectile provides further ivy integration into projectile.
;; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :defer t
  :ensure t
  :after projectile
  :config
  (counsel-projectile-mode))

;; Run ripgrep with Projectile
;; https://github.com/nlamirault/ripgrep.el/blob/master/projectile-ripgrep.el
(use-package projectile-ripgrep
  :after projectile)

;;(global-set-key (kbd "C-x M") 'mu4e)

;; https://lars.ingebrigtsen.no/2014/11/13/welcome-new-emacs-developers/

;; Use Groovy mode for Gradle build files.
(use-package groovy-mode
  :defer t
  :mode (("\\.groovy"    . groovy-mode)
	 ("build.gradle" . groovy-mode)))

;; Use Ibuffer for Buffer List.
;; Some ibuffer tips: http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; https://github.com/purcell/ibuffer-projectile
(add-hook 'ibuffer-hook
	  (lambda ()
	    (ibuffer-projectile-set-filter-groups)
	    (unless (eq ibuffer-sorting-mode 'alphabetic)
	      (ibuffer-do-sort-by-alphabetic))))

;; Python
;;
;; https://github.com/jorgenschaefer/elpy
;; Docs: https://elpy.readthedocs.io/en/latest/index.html
;; pip install jedi flake8 importmagic autopep8 yapf
(use-package elpy
  :ensure t
  :init (with-eval-after-load 'python (elpy-enable))
  :bind (:map elpy-mode-map
              ([f12] . elpy-shell-send-region-or-buffer)
              ("M-." . elpy-goto-definition)))
;; Provides a minor-mode `yapf-mode` that turns on automatically
;; running YAPF on a buffer before saving
;; https://github.com/JorisE/yapfify
(use-package yapfify
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

;; Python auto-completion for Emacs
;; https://github.com/tkf/emacs-jedi
;;(use-package jedi
;;  :init
;;  (add-hook 'python-mode-hook 'jedi:setup)
;;  (add-hook 'python-mode-hook 'flycheck-mode)
;;  :config
;;  (use-package flycheck-mypy)
;;  (setq jedi:complete-on-dot t))


;;(require 'flycheck-mypy)

;; A JavaScript development environment for Emacs.
;; https://github.com/NicolasPetton/Indium
;; https://indium.readthedocs.io/en/latest/
(use-package indium
  :pin melpa
  :defer t)

;; An Emacs web feeds client.
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :defer t
  :ensure t
  :bind ("C-x w" . elfeed)
  :config
  ;; Enhance the user interface a little...
  ;; https://github.com/algernon/elfeed-goodies
  (use-package elfeed-goodies
    :config
    (elfeed-goodies/setup))
  (use-package elfeed-org
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files '("~/org/feeds.org"))))

(global-set-key (kbd "C-x K") 'other-window-kill-buffer)

;; Manage org-mode TODOs for your projectile projects.
;; https://github.com/IvanMalison/org-projectile
(use-package org-projectile
  :ensure t
  :pin elpa
  :bind (("C-c c"   . org-capture)
         ("C-c n p" . org-projectile-project-todo-completing-read))
         
  :config
  (progn
    (setq org-projectile:projects-file "~/org/projects.org")
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; Practice touch/speed typing in Emacs.
;; https://github.com/parkouss/speed-type
;; Executing "M-x speed-type-text"" will start the typing exercise.
(use-package speed-type
  :defer t
  :pin melpa)


(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (smartparens-mode t)
            (rainbow-delimiters-mode t)))


;; Meghanada: Java Development Environment for Emacs.
;; https://github.com/mopemope/meghanada-emacs
(use-package meghanada
  :defer t)

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

;; Haskell
;; https://github.com/haskell/haskell-mode
;; http://haskell.github.io/haskell-mode/manual/latest/
(use-package haskell-mode
  :defer t)

;; Structured editing minor mode for Haskell in Emacs.
;; https://github.com/chrisdone/structured-haskell-mode
(use-package shm
  :defer t
  :pin melpa)

;; LaTeX
(use-package auctex
  :defer t
  :config
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  ;; Compile documents to PDF by default
  (setq TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t))

;; Protocol buffers
(use-package protobuf-mode
  :defer t
  :ensure t)

;; Ruby
;; - ruby-mode is built-in
;; - Flycheck will automatically use Rubocop if installed (gem install rubocop)
;; - Start irb: M-x inf-ruby
;; - Start robe: M-x robe-start
(use-package ruby-mode
  :init
  ;; Enable on-the-fly check with RuboCop (if installed).
  ;; gem install rubocop
  (add-hook 'ruby-mode-hook
            (lambda ()
              (flycheck-mode 1)))
  :config
  ;; Shortcut to start embedded ruby interpreter (irb).
  (use-package inf-ruby
    :ensure t
    :bind ("C-c r r" . inf-ruby))
  ;; Shortcut to activate the Ruby version defined in .ruby-version
  ;; (or .rvmrc).
  (use-package rvm
    :bind ("C-c r a" . rvm-activate-corresponding-ruby))
  ;; Enable on-the-fly syntax check.
  (use-package flymake-ruby
    :pin melpa
    :init
    (add-hook 'ruby-mode-hook #'flymake-ruby-load))
  ;; Robe is a code assistance tool that uses a Ruby REPL subprocess
  ;; with your application or gem code loaded, to provide information
  ;; about loaded classes and modules, and where each method is
  ;; defined.
  ;; https://github.com/dgutov/robe
  (use-package robe
    :ensure t
    :init
    (add-hook 'ruby-mode-hook #'robe-mode)
    (add-hook 'robe-mode-hook #'ac-robe-setup))
  ;; Simple Emacs interface to RuboCop
  ;; https://github.com/bbatsov/rubocop-emacs
  (use-package rubocop
    :init
    (add-hook 'ruby-mode-hook #'rubocop-mode)))

;; A flymake syntax-checker for shell scripts.
;; https://github.com/purcell/flymake-shell
(use-package flymake-shell
  :init
  (add-hook 'sh-set-shell-hook 'flymake-shell-load))

;; Indent, move around and act on code based on indentation (yaml,
;; python, jade, etc).
;; https://gitlab.com/emacs-stuff/indent-tools
(use-package indent-tools
  :init
  (global-set-key (kbd "C-c >") 'indent-tools-hydra/body))

(use-package zeal-at-point
  :if (and window-system (eq system-type 'gnu/linux))
  :pin melpa)

(use-package dash-at-point
  :if (and window-system (eq system-type 'darwin)))

;; Browse Dash docsets using Ivy.
;; https://github.com/nathankot/counsel-dash
(use-package counsel-dash
  :bind ("C-c C-D" . counsel-dash)
  :init
  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Python 3"))))
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Go"))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Emacs Lisp"))))
  (add-hook 'php-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("PHP" "PHPUnit"))))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Markdown"))))
  (add-hook 'apache-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Apache_HTTP_Server"))))
  (add-hook 'dockerfile-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Docker"))))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Clojure" "Java"))))
  (add-hook 'java-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Java"))))
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Haskell"))))
  (add-hook 'swift-mode-hook
            (lambda ()
              (setq-local counsel-dash-docsets '("Swift"))))
  :config
  ;;(setq counsel-dash-common-docsets '("Ansible"))
  (setq counsel-dash-docsets-path "~/.docsets")
  (setq counsel-dash-browser-func 'browse-url))

;; Database Interface for Emacs.
;; https://github.com/kiwanami/emacs-edbi
;; Requires Perl DBI modules:
;;   $ cpan RPC::EPC::Service DBI DBD::SQLite DBD::Pg DBD::mysql
;; Usage:
;;   $ M-x edbi:open-db-viewer
;;   Example URL: DBI:mysql:database=mydb;host=127.0.0.1;port=3306
(use-package edbi
  :pin melpa
  :defer t)

;; Emacs mode for Dockerfiles.
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :ensure t
  :defer t
  :init
  (add-hook 'dockerfile-mode-hook
            (lambda ()
              (let ((line (read-first-line)))
                (if (string-prefix-p "# Project:" line)
                    (let ((project (car (last (split-string line)))))
                      (setq-local docker-image-name project))))))
  (add-hook 'dockerfile-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil))))

;; Major mode for editing docker-compose files.
;; https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode
  :defer t)

;; View certificates and CRLs using OpenSSL in Emacs.
;; https://github.com/jobbflykt/x509-mode
(use-package x509-mode
  :defer t)

;; https://github.com/kyagi/shell-pop-el
(use-package shell-pop
  :ensure t
  :defer t
  :init
  ;; Fix: Emacs25 changed the shell buffers to open in a new window instead of the same one.
  ;; https://github.com/kyagi/shell-pop-el/issues/51
  (push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist))

(use-package git-timemachine
  :ensure t
  :defer t
  :pin melpa
  :bind ("C-x G" . git-timemachine))

;; Multiple cursors for Emacs.
;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :defer t
  :bind (("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-c C->"     . mc/mark-all-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-M->"       . mc/edit-ends-of-lines)
         ("C-M-<"       . mc/edit-beginnings-of-lines)))

;; Increase selected region by semantic units.
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/contract-region)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)))

(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends 'company-shell))

;; Winner mode - undo and redo window configuration
;; Use C-c <left> and C-c <right> to switch between window configurations.
(use-package winner
  :defer t
  :config
  (winner-mode 1))

(use-package el-get
  :ensure t
  :pin melpa
  :commands (el-get-bundle))

;(el-get-bundle transponse-frames
;  :url "https://raw.githubusercontent.com/emacsmirror/emacswiki.org/master/transpose-frame.el")

; Manage docker from Emacs.
; https://github.com/Silex/docker.el
(use-package docker
  :ensure t
  :config
  :bind ("C-c d" . docker))

;; Interactively highlight which buffer is active by dimming the others.
;; https://github.com/gonewest818/dimmer.el
(use-package dimmer
  :ensure t
  :config
  (dimmer-mode 1))

;;(use-package comment-tags
;;  :init
;;  (add-hook))

;; Highlight uncommitted changes.
;; https://github.com/dgutov/diff-hl
;; (use-package diff-hl
;;   :defer t
;;   :ensure t
;;   :pin melpa
;;   :init
;;   (add-hook 'prog-mode-hook #'turn-on-diff-hl-mode)
;;   (add-hook 'vc-dir-mode-hook #'turn-on-diff-hl-mode)
;;   (add-hook 'dired-mode-hook #'diff-hl-dired-mode)
;;   (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

;; Flycheck YAMLLint integration
;; https://github.com/krzysztof-magosa/flycheck-yamllint
(use-package flycheck-yamllint
  :ensure t
  :defer t
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))
    (eval-after-load 'yaml-mode
      '(add-hook 'yaml-mode-hook
                 (lambda ()
                   (flycheck-mode 1))))))

;; Preserve the scratch buffer across Emacs sessions.
;; https://github.com/Fanael/persistent-scratch
(use-package persistent-scratch
  :pin melpa
  :init
  (persistent-scratch-setup-default))

;; Elixir
(use-package alchemist
  :init
  (progn
    (eval-after-load 'elixir-mode
      '(add-hook 'elixir-mode-hook
                 (lambda ()
                   (company-mode 1))))))

;; https://github.com/anildigital/mix-format.el
;;  (add-hook 'elixir-mode-hook
;;            (lambda ()
;;              (add-hook 'before-save-hook 'elixor-format-before-save))))

(use-package sqlup-mode
  :disabled
  :init
  ;; Capitalize keywords in SQL mode
  (add-hook 'sql-mode-hook 'sqlup-mode)
  ;; Capitalize keywords in an interactive session (e.g. psql)
  (add-hook 'sql-interactive-mode-hook 'sqlup-mode)
  ;; Set a global keyword to use sqlup on a region
  (global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)
  (add-to-list 'sqlup-blacklist "User"))

;; Emacs interface to Google Translate
;; https://github.com/atykhonov/google-translate
(use-package google-translate
  :init
  (require 'google-translate-smooth-ui)
  ; Select translation direction using C-n and C-p
  (setq google-translate-translation-directions-alist
        '(("en" . "de") ("de" . "en")))
  (setq google-translate-show-phonetic t)
  :bind ("C-c t" . google-translate-smooth-translate))

;; Enforce rules for popup windows.
;; https://github.com/wasamasa/shackle
;; (use-package shackle
;;   :defer t
;;   :init
;;   (add-to-list 'shackle-rules '("\\*shell-1\\*" :regexp t :same t)))

(use-package bash-completion
  :init
  (bash-completion-setup))

;; Avoid escape nightmares by editing strings in a separate buffer.
;; https://github.com/magnars/string-edit.el
(use-package string-edit
  :ensure t
  :bind ("C-c C-\"" . string-edit-at-point))

;; https://github.com/Fuco1/elisp-docstring-mode
(use-package elisp-docstring-mode
  :ensure t)

;; Source: https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750
(defun ediff-copy-both-to-C ()
  (interactive)
  (let ((a (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer))
        (b (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)))
    (ediff-copy-diff ediff-current-difference nil 'C nil (concat a b))))

  ;; (ediff-copy-diff ediff-current-difference nil 'C nil
  ;;                  (concat
  ;;                   (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
  ;;                   (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
; (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (define-key ediff-mode-map "d" 'ediff-copy-both-to-C)))

;; http://commercialhaskell.github.io/intero/
;; https://github.com/commercialhaskell/intero
(use-package intero
  :disabled
  :init
  (progn
    (eval-after-load 'haskell-mode
      '(add-hook 'haskell-mode-hook
                 (lambda ()
                   (intero-mode 1))))))

;; Integrates eshell with bookmark.el.
;; https://github.com/Fuco1/eshell-bookmark
(use-package eshell-bookmark
  :ensure t
  :init
  (add-hook 'eshell-mode-hook #'eshell-bookmark-setup))

(let* ((etc-dir (expand-file-name "etc" dotfiles-dir))
       (con-file (expand-file-name "sql-conns.el" etc-dir)))
  (if (file-exists-p con-file)
      (load-file con-file)))

;; Drag stuff (words, region, lines) around in Emacs.
;; https://github.com/rejeep/drag-stuff.el
(use-package drag-stuff
  :ensure t
  :diminish drag-stuff-mode
  :bind (("M-S-<up>"    . drag-stuff-up)
         ("M-S-<down>"  . drag-stuff-down)
         ("M-S-<left>"  . drag-stuff-left)
         ("M-S-<right>" . drag-stuff-right))

  :config
  (drag-stuff-global-mode 1))

(use-package forge-github
  :disabled
  :load-path "/home/matthias/projects/foss/forge/lisp")

(use-package forge-bitbucket
  :disabled
  :load-path "/home/matthias/projects/foss/forge/lisp")

;; Keep ~/.emacs.d clean.
;; https://github.com/emacscollective/no-littering
(use-package no-littering
  :disabled)


(use-package slack
  :commands (slack-start)
  :init
  (setq slack-buffer-emojify t)
  (setq slack-prefer-current-team t)
  (setq slack-request-timeout 10000)
  :config
  (slack-register-team
   :name "clojurians"
   :default t
   :client-id "63004d50-1541676708.024&_x_csid=axg3kCXnHPQ"
   :client-secret ""
   :token "xoxs-3883567535-322576429216-367833919651-cc4455a35282673011848dc4a14e5c32cc1638fc760a2d1b322389cae3523829"
   :subscribed-channels '(clojured jobs remote-jobs)
   :full-and-display-names t))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;;(setq epg-gpg-program "gpg2"
