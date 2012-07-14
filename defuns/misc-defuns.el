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