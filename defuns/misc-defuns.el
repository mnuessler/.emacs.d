(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

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
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

;; Format JSON using underscore-cli
;; (https://github.com/ddopson/underscore-cli)
;; Installation:
;; - `brew install node'
;; - `npm install -g underscore-cli'
(defun json-format-underscore ()
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
