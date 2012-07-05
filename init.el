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
;;(require 'setup-ido)
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
(add-to-list 'load-path "~/emacs.d/tramp/lisp/")
(require 'tramp)
(add-to-list 'Info-default-directory-list "~/emacs/tramp/info/")

;; PHP
(add-to-list 'load-path "~/.emacs.d/php-mode/")
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

;; Start Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
