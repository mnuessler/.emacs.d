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

;; Smex
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Find file in project
;;(global-set-key (kbd "C-x f") 'find-file-in-project)

;; php-mode
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

;; kill to start of current line
(global-set-key (kbd "C-;") 'kill-start-of-line)

;; Go to next line from anywhere in the current line
;; (without modifying current line). (Like in Eclipse IDE)
(global-set-key (kbd "<S-return>") (kbd "C-e C-m"))

;; Django
;;(global-set-key (kbd "C-x j") 'python-django-open-project)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(provide 'key-bindings)
