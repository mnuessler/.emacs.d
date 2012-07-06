;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" dotfiles-dir))

;; Set up load path
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path site-lisp-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" dotfiles-dir))

; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" dotfiles-dir))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Setup extensions
(require 'setup-ido)
(require 'setup-yasnippet)
(require 'setup-dired)
(require 'setup-magit)
;;(require 'setup-ace-jump-mode)
(require 'setup-markdown-mode)
(require 'setup-org-mode)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#111122")

;; Map files to modes
(require 'mode-mappings)

;; Initialize smex, a smart Emacs enhancement
(require 'smex)
(smex-initialize)

;; Setup key bindings
(require 'key-bindings)

;; Misc
(require 'appearance)
;;(require 'misc)
(when is-mac (require 'mac))

;; Load dired-extra library
(add-hook 'dired-load-hook
	  (function (lambda () (load "dired-x"))))

;; Reuse directory buffers in dired (provided by dired-plus)
;; see http://emacswiki.org/emacs/DiredReuseDirectoryBuffer
(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)

;; Table
(require 'table)
(add-hook 'text-mode-hook 'table-recognize)

;; Enable recent files
(require 'recentf)
(recentf-mode 1)

(require 'diminish)
(diminish 'yas/minor-mode)
;;(eval-after-load "filladapt" '(diminish 'filladapt-mode))

;; TRAMP
;;(if (eq system-type 'darwin)
(require 'tramp)
(add-to-list 'Info-default-directory-list "~/.emacs.d/site-lisp/tramp/info/")

;; PHP
(require 'php-mode)

;; Zen-coding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; Expand region
(require 'expand-region)

;; Mark multiple
(require 'inline-string-rectangle)

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(require 'mark-more-like-this)

;; Ace Jump Mode
(require 'ace-jump-mode)

;; Thunderbird email with external editor
(require 'tbemail)
;;(add-hook 'tbemail-mode-hook 'turn-on-auto-fill)

(require 'thesaurus)
(setq thesaurus-bhl-api-key "5c4736a087cad9dd0b899b227aa46796")  ;; from registration

;; Don't highlight text between quotes over multiple lines for properties files
(add-hook 'conf-javaprop-mode-hook 
          '(lambda () (conf-quote-normal nil)))

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; Start Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(csv-separators (quote (",")))
 '(erc-nick "mnuessler")
 '(erc-server "irc.iscout.local")
 '(erc-user-full-name "Matthias Nuessler"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
