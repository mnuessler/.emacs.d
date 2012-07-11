;; Adjust frame size
(if (window-system)
    (set-frame-size (selected-frame) 100 70))

;; No blinking cursor
(blink-cursor-mode 0)

;; Display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Highlight current line and column when idle
;; Requires dependencies hl-line+.el vline.el col-highlight.el
(require 'crosshairs)
(toggle-crosshairs-when-idle 1)

;; No scroll bars and no tool bar
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Built-in theming support available starting with Emacs 24
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(load-theme 'zenburn t)

;; Collection of color themes
;; Screenshots of available themes at
;; http://gnuemacscolorthemetest.googlecode.com/svn/html/index-el.html
(if (window-system)
    (progn
      (require 'color-theme)
      (eval-after-load "color-theme"
	'(progn
	   (color-theme-initialize)
	   (color-theme-comidia)))))

;; Whitespace, configured via Custom
(require 'whitespace)

(provide 'appearance)
