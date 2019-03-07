(require 'org)

;; Enable auto-indent
(setq org-startup-indented t)
(setq org-indent-mode t)
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on

;; (add-hook 'org-mode-hook (lambda()
;; 	  (progn
;; 	    (toggle-crosshairs-when-idle 0)
;; 	    (toggle-hl-line-when-idle 1))))

;; Log time when closing TODO items
(setq org-log-done 'time)
;;(setq org-log-done 'note)

;; Define 'workflow'
(setq org-todo-keywords
      '((sequence "TODO" "STARTED" "WAITING" "|" "DONE" "CANCELED")))
(setq org-todo-keyword-faces
      '(("TODO"     . org-warning)
        ("STARTED"  . "yellow")
        ("WAITING"  . "blue")
        ("CANCELED" . (:foreground "blue" :weight bold))))

;; Persist clock time between sessions
(setq org-clock-persist 'history)
;; Deactivated because of problem with Emacs 24 on Linux:
;; "cant't find library org"
;;(org-clock-persistence-insinuate)
(setq org-clock-into-drawer t)

;; MobileOrg
;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-agenda-files
      '("~/Dropbox/org/private.org"
        "~/Dropbox/org/dev.org"
        "~/Dropbox/org/quotations.org"))

;; Settings for code blocks
;; Enable syntax highlighting
(setq org-src-fontify-natively t)
;; Preserve indentation
(setq org-src-preserve-indentation t)
;; Mapping of languages to major modes
(add-to-list 'org-src-lang-modes '("json" . javascript))

;; Enable speed commands
(setq org-use-speed-commands t)

;;(org-babel-do-load-languages
;; 'org-babel-load-languages
;; '((plantuml . t)
;;   (restclient . t)))

(setq org-plantuml-jar-path "~/bin/plantuml.jar")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((restclient . t)
   ;;(plantuml . t)
   ;;(influxdb . t)
   ;;(ipython . t)
   ;;(prolog . t)
   ;;(elxir . t)
   ;;(mongo . t)
   ;;(shell . t)
   ;;(redis . t)
   ;;(http . t)
   ;;(rust . t)
   (sql . t)
   ;; https://github.com/pope/ob-go
   ;;(go . t)
 ))

(provide 'setup-org-mode)
