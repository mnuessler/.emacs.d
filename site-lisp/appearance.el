;; Adjust frame size
(if (window-system)
    (set-frame-size (selected-frame) 100 70))

;; No scroll bars and no tool bar, please.
(scroll-bar-mode 0)
(tool-bar-mode 0)

;; No startup screen.
(setq inhibit-startup-screen t)

;; No blinking cursor
(blink-cursor-mode 0)

;; Enable powerline.
(powerline-default-theme)

;; Display line and column numbers
;; TODO Maybe not needed anymore when using powerline
;;(setq line-number-mode t)
;;(setq column-number-mode t)


;; Highlight current line and column when idle
;; Requires dependencies hl-line+.el vline.el col-highlight.el
;;(require 'crosshairs)
;;(toggle-crosshairs-when-idle 1)

;; Theme.
(load-theme 'zenburn t)

;; Whitespace, configured via Custom
;;(require 'whitespace)

;; Use visible bell instead of beep
(setq visible-bell 1)
;; or turn off bell completely
;;(setq ring-bell-function 'ignore)

;; Show matching parenthesis, bracket or brace for the character at point.
(setq show-paren-mode t)
(setq show-paren-delay 0)

(provide 'appearance)
