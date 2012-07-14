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

;; Keep Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
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

;; Setup extensions
(require 'setup-ido)
(require 'setup-yasnippet)
(require 'setup-dired)
(require 'setup-magit)
;;(require 'setup-ace-jump-mode)
(require 'setup-markdown-mode)
(require 'setup-org-mode)
(require 'setup-php-mode)

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
(diminish 'global-whitespace-mode)
;;(eval-after-load "filladapt" '(diminish 'filladapt-mode))

;; TRAMP
;;(if (eq system-type 'darwin)
(require 'tramp)
(add-to-list 'Info-default-directory-list "~/.emacs.d/site-lisp/tramp/info/")

;; Zen-coding
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

;; Expand region
(require 'expand-region)

;; Mark multiple
(require 'inline-string-rectangle)

(require 'mark-more-like-this)

(require 'find-file-in-project)

;; Ace Jump Mode
(require 'ace-jump-mode)

;; Thunderbird email with external editor
(require 'tbemail)
(add-hook 'tbemail-mode-hook 'turn-off-auto-fill)

;; Thesaurus
(require 'thesaurus)
(setq thesaurus-bhl-api-key "5c4736a087cad9dd0b899b227aa46796")  ;; from registration

;; Don't highlight text between quotes over multiple lines for properties files
(add-hook 'conf-javaprop-mode-hook
          '(lambda () (conf-quote-normal nil)))

(require 'sunrise-commander)

;; Annoying arrows mode
(require 'annoying-arrows-mode)
(global-annoying-arrows-mode)
;; Use visible bell instead of beep
(setq visible-bell 1)
;; or turn off bell completely
;;(setq ring-bell-function 'ignore)

;; Show matching parenthesis, bracket or brace for the character at point.
(setq show-paren-mode t)
(setq show-paren-delay 0)

(require 'shell-command)
(shell-command-completion-mode)

;; source: http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunt one"
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
(global-set-key (kbd "<f2>") 'visit-ansi-term)

;; make script files executable automatically
;; source: http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

;; Start Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
