(defun show-current-buffer-file-name ()
  "Show the absolute path of the file in the current buffer."
  (interactive)
  (if buffer-file-name
      (let ((file-name (file-truename buffer-file-name)))
        (message file-name)
        (kill-new file-name))
    (message "*** Buffer does not have a file ***")))

;; open PHP manual with lynx, other URLs in Firefox
(defun browse-url-choose-browser (url &optional new-buffer)
  (let ((is-php-manual (string-match "/php-manual/" url)))
    (if is-php-manual (browse-url-text-emacs url new-buffer)
       (if is-mac (browse-url-default-macosx-browser url new-buffer)
	 (browse-url-firefox url new-buffer)))))

(defun gnome-open-file (filename)
  "gnome-opens the specified file."
  (interactive "file to open: ")
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/gnome-open" filename)))

(defun mac-open-file (filename)
  "mac-open the specified file."
  (interactive "file to open: ")
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/open" filename)))

(defun dired-gnome-or-mac-open-file ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if is-mac (mac-open-file file)
      (gnome-open-file (file)))))

(defun json-format-py ()
  "Format JSON (using Python)."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; Format JSON using underscore-cli
;; (https://github.com/ddopson/underscore-cli)
;; Installation:
;; - `brew install node'
;; - `npm install -g underscore-cli'
(defun json-format-underscore ()
  "Format JSON (using underscore)."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "underscore pretty" (buffer-name) t)))

;; Kill buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
	  (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
	    (kill-buffer buffer)))
	(buffer-list)))

(defun other-window-kill-buffer ()
  "Kill the buffer in the other window"
  (interactive)
  ;; Window selection is used because point goes to a different window
  ;; if more than 2 windows are present
  (let ((win-curr (selected-window))
        (win-other (next-window)))
    (select-window win-other)
    (kill-this-buffer)
    (select-window win-curr)))

(defun read-first-line ()
  "Read the first line of the buffer"
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (line-beginning-position)
                                    (line-end-position))))

;; Source: https://www.bennee.com/~alex/blog/2018/04/07/working-with-dired/
;; alias dired="emacsclient -a '' -t -e '(my-dired-frame default-directory)'"
(defun my-dired-frame (directory)
  "Open up a dired frame which closes on exit."
  (interactive)
  (switch-to-buffer (dired directory))
  (local-set-key
   (kbd "C-x C-c")
   (lambda ()
     (interactive)
     (kill-this-buffer)
     (save-buffers-kill-terminal 't))))

;; Source: http://www.enigmacurry.com/2008/12/26/emacs-ansi-term-tricks/
(require 'term)
(defun visit-ansi-term ()
  "If the current buffer is:
     1) a running ansi-term named *ansi-term*, rename it.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunct one"
  (interactive)
  (let ((is-term (string= "term-mode" major-mode))
        (is-running (term-check-proc (buffer-name)))
        (term-cmd "/bin/bash")
        (anon-term (get-buffer "*ansi-term*")))
    (if is-term
        (if is-running
            (if (string= "*ansi-term*" (buffer-name))
                (call-interactively 'rename-buffer)
              (if anon-term
                  (pop-to-buffer "*ansi-term*")
                (ansi-term term-cmd)))
          (kill-buffer (buffer-name))
          (ansi-term term-cmd))
      (if anon-term
          (if (term-check-proc "*ansi-term*")
              (pop-to-buffer "*ansi-term*")
            (kill-buffer "*ansi-term*")
            (ansi-term term-cmd))
        (ansi-term term-cmd)))))

;; Switch perspectives when switching projects, but use ivy for the
;; selection. Source:
;; https://github.com/syl20bnr/spacemacs/blob/bd7ef98e4c35fd87538dd2a81356cc83f5fd02f3/layers/%2Bspacemacs/spacemacs-layouts/funcs.el#L352
(defun spacemacs/ivy-persp-switch-project (arg)
  (interactive "P")
  (ivy-read "Switch to Project Perspective: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action (lambda (project)
                      (let ((persp-reset-windows-on-nil-window-conf t))
                        (persp-switch project)
                        (let ((projectile-completion-system 'ivy))
                          (projectile-switch-project-by-name project))))))

(defun my/unix-timestamp-to-date ()
  "Prompts for a unix epoch time stamp and converts it to a human-readable date."
  (interactive)
  (let* ((time-zone "UTC")
         (time-unix (seconds-to-time (read-number "Unix epoch time stamp: ")))
         (time-str (format-time-string "<%Y-%m-%d %a %H:%M:%S>" time-unix time-zone)))
    (message "Date: %s (%s)" time-str time-zone)))


;; Source: https://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version/29757750#29757750
(defvar ediff-current-difference)
(defvar ediff-control-buffer)
(defun ediff-copy-both-to-C ()
  "Copies the changes from both diff buffers A and B into the result buffer C."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(add-hook 'ediff-keymap-setup-hook
          (lambda ()
            (define-key ediff-mode-map "B" #'ediff-copy-both-to-C)))

;; https://gist.github.com/nriley/1283061
(defun toggle-frame-fullscreen (&optional frame)
  (interactive)
  (set-frame-parameter frame 'fullscreen
                       (if (null (frame-parameter frame 'fullscreen))
                           'fullboth nil)))
; ediff-toggle-wide-display tries to use the entire display width,
; which breaks with multiple monitors
(defun ediff-fullscreen-as-wide-display ()
  "Try to use the entire display width, which breaks with multiple monitors."
  (interactive)
  (if (eq window-system 'mac)
      (defadvice ediff-toggle-wide-display
        (around ediff-toggle-frame-fullscreen activate)
        (toggle-frame-fullscreen (window-frame ediff-window-A))
        (raise-frame ediff-control-frame))))

;; Sorts the variable containing the selected packages alphabetically
;; and adds the result to the kill-ring, so that it can be pasted back
;; into custom.el. Reasoning: Keeping the list sorted makes it easier
;; to resolve merge conflicts in custom.el. (Works for now, but
;; ideally it would be sorted automatically upon save.)
(defun sort-selected-packages-and-add-to-kill-ring ()
  (interactive)
  (kill-new (format "%s" (cl-sort package-selected-packages 'string-lessp))))

(defun insert-current-date ()
  "Insert the current date (i.e. '2021-02-03')."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
