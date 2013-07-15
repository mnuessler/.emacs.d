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

(defun json-format ()
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))
