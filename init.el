;; First the package management.
(package-initialize)

(setq package-archives '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
                         ("Marmalade"    . "https://marmalade-repo.org/packages/")
                         ("MELPA"        . "https://melpa.milkbox.net/packages/")
			 ("MELPA Stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("MELPA Stable" . 15)
				   ("Marmalade"    . 10)
				   ("GNU ELPA"     . 5)
				   ("MELPA"        . 0)))

;; Determine path to ".emacs.d" directory.
(setq dotfiles-dir (file-name-directory
		    (or (buffer-file-name) load-file-name)))

;; Keep Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" (expand-file-name "site-lisp" dotfiles-dir)))
(load custom-file)

;; Write backup files to own directory
(setq backup-directory-alist `(("." . ,(expand-file-name
					(concat dotfiles-dir "backups")))))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Set up load path
(add-to-list 'load-path (expand-file-name "site-lisp" dotfiles-dir))

(require 'appearance)

(use-package smooth-scrolling
  :defer 0
  :config
  (smooth-scrolling-mode 1))

(use-package browse-kill-ring
  :config
  ;; Make M-y to use browse-kill-ring.
  (browse-kill-ring-default-keybindings))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :diminish undo-tree-mode)

(use-package bookmark+)

(use-package gitconfig-mode
  :defer t)

;; (use-package back-button
;;   :config
;;   (back-button-mode 1))

(use-package window-numbering
  :config
  (window-numbering-mode 1))

(use-package recentf
  :config
  (recentf-mode 1))

;; Get side-by-side diffs
(setq ediff-split-window-function 'split-window-horizontally)

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (use-package magit-gerrit))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview
             kubernetes-display-pods
             kubernetes-display-configmaps
             kubernetes-display-secrets))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-mc
  :bind (("C-)"   . ace-mc-add-multiple-cursors)
	 ("C-M-)" . ace-mc-add-single-cursor)))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config (exec-path-from-shell-initialize))

;; (require 'server)
;; (use-package edit-server
;;   :if window-system
;;   :init
;;   (add-hook 'after-init-hook '(lambda ()
;; 				(unless (server-running-p)
;; 				  (server-start))) t)
;;   (add-hook 'after-init-hook 'edit-server-start t))

;; Required by neotree icon theme and all-the-icons-dired-mode.
;; Requires installation of fonts to work correctly:
;; https://github.com/domtronn/all-the-icons.el/tree/master/fonts
(use-package all-the-icons
  :diminish all-the-icons-dired-mode)

;; Use icons in dired mode.
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

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
  :diminish ivy)

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
  :init
  ;; folders
  (setq mu4e-maildir "/home/matthias/Maildir"
	mu4e-drafts-folder "/[Gmail].Drafts"
	mu4e-sent-folder   "/[Gmail].Sent Mail"
	mu4e-trash-folder  "/[Gmail].Trash")
  ;; don't automatically mark messages as read
  (setq mu4e-view-auto-mark-as-read nil)
  (setq mu4e-html2text-command "html2text -utf8 -width 72")
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
  ;; 	       (make-mu4e-bookmark
  ;; 		:name  "Big messages"
  ;; 		:query "size:5M..500M"
  ;; 		:key ?b))
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
;;	(dolist (buffer (buffer-list t))
;;	  (set-buffer buffer)
;;	  (when (and (derived-mode-p 'message-mode)
;;		     (null message-sent-message-via))
;;	    (push (buffer-name buffer) buffers))))
;;      (nreverse buffers)))

;;  (setq gnus-dired-mail-mode 'mu4e-user-agent)
;;  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode))


;; http://www.djcbsoftware.nl/code/mu/mu4e/Other-search-functionality.html#Including-related-messages

;; Automatically detect language for Flyspell.
(use-package guess-language
  :ensure t
  :defer t
  :init
  (add-hook 'text-mode-hook #'guess-language-mode)
  :config
  (setq guess-language-langcodes '((en . ("en_US" "English"))
                                   (de . ("de_DE" "German")))
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
  :init
  (setq ansible::vault-password-file "~/.ansible-vault-pass")
  :config
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)
  (add-hook 'ansible-hook #'ansible-doc-mode))

(require  'key-bindings)


(use-package tramp)

(use-package dired-x)

(use-package dired
  :init
  ;; Set to non-nil to enable recursive deletion of directories
  (setq dired-recursive-deletes t)
  ;; move files or directories into the operating system's Trash, instead of deleting them outright
  (setq delete-by-moving-to-trash t)
  ;; let search commands limit themselves to the file names (C-s behaves like M-s f C-s),
  ;; but only when point was on a file name initially
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
  ;; don't create new buffer each time moving up a directory
  ;; source: http://www.emacswiki.org/emacs-es/DiredReuseDirectoryBuffer
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (define-key dired-mode-map (kbd "^")
		(lambda () (interactive) (find-alternate-file "..")))
	      ))
  ;; dired-sync provides a simple and easy way to synchronize directories
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (when (require 'dired-sync nil t)
		(define-key dired-mode-map (kbd "C-c S") 'dired-do-sync))))
  ;; bound to `E': open file with gnome-open or mac open 
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (local-set-key "E" 'dired-gnome-or-mac-open-file))))

(use-package yasnippet
  :config
  (yas/global-mode 1)
  :diminish yas-minor-mode)

(use-package restclient)

;; Major mode for PlantUML diagram definitions.
;; http://plantuml.com/
(use-package plantuml-mode
  :mode "\\.plu\\'")

;; TODO mode mappings
(require 'setup-org-mode)

(use-package highlight-indentation-mode
  :diminish highlight-indentation-mode)

(use-package yaml-mode
  :init
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode))

;; Open _external_ terminal in current directory or project.
;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :bind (("C-<f5>" . terminal-here-launch)
	 ("C-<f6>" . terminal-here-project-launch)))

(use-package cider)

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

(use-package markdown-mode
  :mode (("\\.md$"       . markdown-mode)
	 ("\\.markdown$" . markdown-mode))
  :config
  ;; Check balance of parantheses upon save.
  (add-hook 'markdown-mode-hook
	    (lambda ()
	      (when buffer-file-name
		(add-hook 'after-save-hook 'check-parens nil t)))))

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
;; source: http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Source: http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunct one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (pop-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (pop-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

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

;; Haskell
;; https://github.com/haskell/haskell-mode
;; http://haskell.github.io/haskell-mode/manual/latest/
(use-package haskell-mode
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

;; GNU Octave
;; apt-get install octave
(use-package octave-mode
  :config
  (abbrev-mode 1)
  (auto-fill-mode 1)
  (if (eq window-system 'x)
      (font-lock-mode 1))
  (octave-auto-indent 1)
  (octave-auto-newline 1)
  (RET-behaves-as-LFD))
