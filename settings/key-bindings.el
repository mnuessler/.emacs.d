;; Keyboard shortcut for toggling autofill mode
;;(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; Kill whole line.
(global-set-key (kbd "C-S-k") #'kill-whole-line)

;; Kill to start of current line.
(global-set-key (kbd "C-c k") #'kill-start-of-line)

;; Mark current word
;; defined in editing-defuns.el
(global-set-key (kbd "C-c w") #'mark-current-word)

;; org-mode
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c b") #'org-iswitchb)

;; Smex
;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;;(define-key php-mode-map (kbd "RET") 'newline-and-indent)

;; show buffer file name in mini-buffer
(global-set-key (kbd "C-c z") #'show-current-buffer-file-name)

;; (Package iso-insert has become obsolete.)
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

;; Remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                #'smarter-move-beginning-of-line)

;; Duplicate line
(global-set-key (kbd "C-S-d") #'duplicate-line)
;; TODO key binding for move-line-up/move-line-down

;; Bind a key for imenu, but use counsel.
(global-set-key (kbd "M-i")     #'counsel-imenu)
(global-set-key (kbd "C-x C-r") #'counsel-recentf)

;; Expand current window to use half of the other window's lines.
(global-set-key (kbd "C-c v") 'halve-other-window-height)

;; Scroll by line
;; Source: https://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs
(global-set-key (kbd "M-<down>") #'scroll-up-line)
(global-set-key (kbd "M-<up>")   #'scroll-down-line)

;; Toggle fullscreen (function in misc-defuns.el)
(global-set-key (kbd "C-x 5 9") #'toggle-frame-fullscreen)

;; Clean-up buffer
(global-set-key (kbd "C-c n") #'cleanup-buffer)

(provide 'key-bindings)
