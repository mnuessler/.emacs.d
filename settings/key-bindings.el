;; Keyboard shortcut for toggling autofill mode
;;(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Kill whole line.
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; Kill to start of current line.
(global-set-key (kbd "C-c k") 'kill-start-of-line)

;; Mark current word
;; defined in editing-defuns.el
(global-set-key "\C-cw" 'mark-current-word)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Smex
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;(define-key php-mode-map (kbd "RET") 'newline-and-indent)

;; show buffer file name in mini-buffer
(global-set-key "\C-cz" 'show-file-name)

;; insert German umlauts without switching the keyboard layout
;; (require 'iso-insert)
;; (global-set-key (kbd "M-;") 'insert-o-umlaut)
;; (global-set-key (kbd "M-:") 'insert-O-umlaut)
;; (global-set-key (kbd "M-'") 'insert-a-umlaut)
;; (global-set-key (kbd "M-\"") 'insert-A-umlaut)
;; (global-set-key (kbd "M-[") 'insert-u-umlaut)
;; (global-set-key (kbd "M-{") 'insert-U-umlaut)
;; (global-set-key (kbd "M--") 'insert-ss)

;; Go to next line from anywhere in the current line
;; (without modifying current line). (Like in Eclipse IDE)
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))

;; Django
;;(global-set-key (kbd "C-x j") 'python-django-open-project)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Duplicate line
(global-set-key (kbd "C-S-d") 'duplicate-line)

;; Bind a key for imenu, but use counsel.
(global-set-key (kbd "M-i") 'counsel-imenu)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)

(provide 'key-bindings)
