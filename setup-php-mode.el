(defun wicked/php-mode-init ()
  "Set some buffer-local variables."
  ;; Set this to t if you want case-insensitive search.
   (setq case-fold-search t)
   ;; Set this to nil if you want to insert spaces instead of tabs
   (setq indent-tabs-mode nil)
   (setq fill-column 78)
   ;; Set your tab size or number of spaces used as a basis for indentation
   (setq c-basic-offset 4)
;;   (c-set-offset 'arglist-cont 0)
;;   (c-set-offset 'arglist-intro '+)
;;   (c-set-offset 'case-label 4)
;;   (c-set-offset 'arglist-close 0)
   )
(add-hook 'php-mode-hook 'wicked/php-mode-init)

;; enable on-the-fly syntax checking
(add-hook 'php-mode-hook '(lambda ()
	  (flymake-mode 1)))

(provide 'setup-php-mode)
