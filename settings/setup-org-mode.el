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
      '((sequence "TODO(t)" "IN-PROGRESS(p)" "WAITING(w@/!)" "|" "DONE(d!)")
        (sequence "|" "CANCELED(c@)")))
(setq org-todo-keyword-faces
      '(("TODO"         . org-warning)
        ("IN-PROGRESS"  . "yellow")
        ("WAITING"      . "blue")
        ("CANCELED"     . (:foreground "blue" :weight bold))))

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
;;(setq org-agenda-files '("~/Dropbox/org/"))
(setq org-agenda-files '("~/Dropbox/org/private.org"
        "~/Dropbox/org/dev.org"
        "~/Dropbox/org/quotations.org"
        "~/Dropbox/org/todo.org"))

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

(global-set-key (kbd "C-c c") 'org-capture)

;; Templates 'p' and 'L' are for use of
;; https://github.com/sprig/org-capture-extension
;; (Extension requires `brew install emacs-client')
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "inbox.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("p" "Protocol" entry (file+headline "inbox.org" "Inbox")
         "* %^{Title}\nSource: %u, %c\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
	("L" "Protocol Link" entry (file+headline "inbox.org" "Inbox")
         "* %? [[%:link][%:description]] \nCaptured On: %U")))

;; Agenda view as suggested by:
;; https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
(defun air-org-skip-subtree-if-priority (priority)
  "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(defun air-org-skip-subtree-if-habit ()
  "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (string= (org-entry-get nil "STYLE") "habit")
        subtree-end
      nil)))

(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))
        ("o" "At the office" tags-todo "@office"
         ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
         ((org-agenda-overriding-header "Office")))
        ("h" "At home" tags-todo "@home"
         ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
         ((org-agenda-overriding-header "Home")))
        ("D" "Upcoming deadlines" agenda ""
                ((org-agenda-time-grid nil)
                 (org-deadline-warning-days 365)
                 (org-agenda-entry-types '(:deadline))))
        ("p" . "Priorities")
        ("pa" "A items" tags-todo "+PRIORITY=\"A\"")
        ("pb" "B items" tags-todo "+PRIORITY=\"B\"")
        ("pc" "C items" tags-todo "+PRIORITY=\"C\"")))

;; Show next 7 days in agenda instead of current week.
(setq org-agenda-start-on-weekday nil)

(provide 'setup-org-mode)
