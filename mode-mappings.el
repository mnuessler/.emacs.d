;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-hook 'markdown-mode-hook (lambda () (require 'setup-markdown-mode)))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; PHP
(autoload 'php-mode "php-mode")
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Clojure
(autoload 'clojure-mode "clojure-mode")
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
;;(add-hook 'clojure-mode-hook (lambda () (require 'setup-clojure-mode)))

;; JavaScript
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . javascript-mode))
;;(add-hook 'js2-mode-hook (lambda () (require 'setup-js2-mode)))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.emacs.d/snippets" . snippet-mode))

;; Apache mode
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))

;; Taskjuggler mode
(autoload 'taskjugger-mode "taskjuggler-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.tjp\\'" . taskjugger-mode))

;; csv-mode
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))

;; jira-markup-mode
(add-to-list 'auto-mode-alist '("\\.confluence$" . jira-markup-mode))
(add-to-list 'auto-mode-alist '("/itsalltext/.*jira.*\\.txt$" . jira-markup-mode))

;; yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; ruby-mode
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))

;; rhtml-mode
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rjs\\'" . rhtml-mode))

;; octave
(autoload 'octave-mode "octave-mod" nil t)
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; disable auto-fill when using Firefox add-on "It's All Text!"
(add-hook 'text-mode-hook
	  (lambda ()
	    (when (string-match "/itsalltext/" (buffer-file-name))
	      (turn-off-auto-fill))))

(provide 'mode-mappings)
