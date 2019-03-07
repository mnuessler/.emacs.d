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
;; Just flash the mode line which is more subtle.
(setq ring-bell-function
      (lambda ()
	(invert-face 'mode-line)
	(run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Show matching parenthesis, bracket or brace for the character at point.
(setq show-paren-mode t)
(setq show-paren-delay 0)

;; Use Fira Code font
;; (when (window-system)
;;   (set-frame-font "Fira Code"))

;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))

;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))

(provide 'appearance)
