;; Keyboard shortcut for toggling autofill mode
;;(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Sets `C-c d` to `M-x kill-whole-line`
(global-set-key "\C-cd" 'kill-whole-line)

;; Mark current word
;; defined in editing-defuns.el
(global-set-key "\C-cw" 'mark-current-word)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Replace current word by synonym using a thesaurus
(define-key global-map (kbd "C-x t") 'thesaurus-choose-synonym-and-replace)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-+") 'er/contract-region)

;; replace the original string-rectangle binding
;; by the inline version from mark-multiple
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

;; bindings for mark-more-like-this
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; ace-jump-mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; If you also use viper mode :
;;(define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)
;; If you use evil
;;(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

;; Magit
(global-set-key (kbd "C-x m") 'magit-status)

;; Smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Find file in project
(global-set-key (kbd "C-x f") 'find-file-in-project)

;; php-mode
;;(define-key php-mode-map (kbd "RET") 'newline-and-indent)

;; eproject
;; eproject global bindings
(defmacro .emacs-curry (function &rest args)
  `(lambda () (interactive)
     (,function ,@args)))

(defmacro .emacs-eproject-key (key command)
  (cons 'progn
        (loop for (k . p) in (list (cons key 4) (cons (upcase key) 1))
              collect
              `(global-set-key
                (kbd ,(format "C-x p %s" k))
                (.emacs-curry ,command ,p)))))

(.emacs-eproject-key "k" eproject-kill-project-buffers)
(.emacs-eproject-key "v" eproject-revisit-project)
(.emacs-eproject-key "b" eproject-ibuffer)
(.emacs-eproject-key "o" eproject-open-all-project-files)

;; show buffer file name in mini-buffer
(global-set-key "\C-cz" 'show-file-name)

;; insert German umlauts without switching the keyboard layout
(require 'iso-insert)
(global-set-key (kbd "M-;") 'insert-o-umlaut)
(global-set-key (kbd "M-:") 'insert-O-umlaut)
(global-set-key (kbd "M-'") 'insert-a-umlaut)
(global-set-key (kbd "M-\"") 'insert-A-umlaut)
(global-set-key (kbd "M-[") 'insert-u-umlaut)
(global-set-key (kbd "M-{") 'insert-U-umlaut)
(global-set-key (kbd "M--") 'insert-ss)

;; kill to start of current line
(global-set-key (kbd "C-;") 'kill-start-of-line)

;; Go to next line from anywhere in the current line
;; (without modifying current line). (Like in Eclipse IDE)
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))

;; Django
(global-set-key (kbd "C-x j") 'python-django-open-project)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(provide 'key-bindings)
