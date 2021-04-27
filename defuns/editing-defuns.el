;; Mark current word
;; Source: http://emacswiki.org/emacs/MarkCommands
(defun mark-current-word (&optional arg allow-extend)
  "Put point at beginning of current word, set mark at end."
  (interactive "p\np")
  (setq arg (if arg arg 1))
  (if (and allow-extend
	   (or (and (eq last-command this-command) (mark t))
	       (region-active-p)))
      (set-mark
       (save-excursion
	 (when (< (mark) (point))
	   (setq arg (- arg)))
	 (goto-char (mark))
	 (forward-word arg)
	 (point)))
    (let ((wbounds (bounds-of-thing-at-point 'word)))
      (unless (consp wbounds)
	(error "No word at point"))
      (if (>= arg 0)
	  (goto-char (car wbounds))
	(goto-char (cdr wbounds)))
      (push-mark (save-excursion
		   (forward-word arg)
		   (point)))
      (activate-mark))))


;; Cycle conversion between CamelCase under_score etc. styles
;; Source: http://www.emacswiki.org/CamelCase
(defun split-name (s)
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun camelcase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))

(defun camelscore (s)
  (cond ((string-match-p "\:" s) (camelcase  s))
	((string-match-p "-"  s) (colonize   s))
	((string-match-p "_"  s) (dasherize  s))
	(t                       (underscore s))))

(defun camelscore-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
	 (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
	 (end (and (skip-chars-forward  "[:alnum:]:_-") (point)))
	 (txt (buffer-substring beg end))
	 (cml (camelscore txt)) )
    (if cml (progn (delete-region beg end) (insert cml))) ))

;; Standard Emacs convention is that <RET> (aka C-m) just adds a
;; newline, whereas <LFD> (aka C-j) adds a newline and indents
;; it. This is particularly inconvenient for users with keyboards
;; which do not have a special <LFD> key at all; in such cases, it is
;; typically more convenient to use <RET> as the <LFD> key (rather
;; than typing C-j).  (From Octave doc.)
(defun RET-behaves-as-LFD ()
  (let ((x (key-binding "\C-j")))
    (local-set-key "\C-m" x)))

(defun kill-start-of-line ()
  "Kill from point to start of line."
  (interactive)
  (kill-line 0))

(defun normalize-space-in-region (beg end)
  "Replace all whitespace in the region from BEG to END with single spaces."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "\\s-+" nil t)
        (replace-match " ")))))

;; Smarter Navigation to the Beginning of a Line
;; See: http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; URL-encode/-decode region
;; (from http://stackoverflow.com/questions/611831/how-to-url-decode-a-string-in-emacs-lisp)
(defun func-region (start end func)
  "Execute function FUNC over the region between START and END in current buffer."
  (save-excursion
    (let ((text (delete-and-extract-region start end)))
      (insert (funcall func text)))))

(defun urlencode-region (start end)
  "URL-encode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun urldecode-region (start end)
  "URL-decode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

;; Similar to `zap-to-char' but excludes the specified character.
;; Kills text upto and excluding the specified character.
;; Taken from the sample chapter of the `Emacs Mastery' book.
(defun zap-upto-char (arg char)
  "Kill up to but not including ARGth occurrence of CHAR.

Case is ignored if `case-fold-search' is non-nil in the current
buffer.  Goes backward if ARG is negative; error if CHAR not
found."
  (interactive
   (list (prefix-numeric-value current-prefix-arg)
         (read-char "Zap upto char: " t)))
  (zap-to-char arg char)
  (insert-char char)
  (backward-char))

;; Move line
;; Source: https://www.emacswiki.org/emacs/MoveLine
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))

(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

;; Duplicate line
(defun duplicate-line()
  "Duplicate the current line."
  (interactive)
  (save-column
   (move-beginning-of-line 1)
   (kill-line)
   (yank)
   (open-line 1)
   (forward-line 1)
   (yank)))

;; Switch focus to minibuffer window.
;; Source: https://superuser.com/questions/132225/how-to-get-back-to-an-active-minibuffer-prompt-in-emacs-without-the-mouse
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "<f7>") 'switch-to-minibuffer-window)

;; https://emacs.stackexchange.com/questions/41222/how-can-i-pass-the-no-line-break-argument-to-base64-encode-region-in-m-x
(defun my/base64-encode-region-no-break ()
  (interactive)
  (base64-encode-region (mark) (point) t))

(defun base64-encode-region-prefix-arg (&rest _args)
  "Pass prefix arg as third arg to `base64-encode-region'."
  (interactive "r\nP"))
(advice-add 'base64-encode-region :before #'base64-encode-region-prefix-arg)

;; Source: http://ergoemacs.org/emacs/elisp_escape_quotes.html
(defun my/escape-quotes (@begin @end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `my/unescape-quotes'"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun my/unescape-quotes (@begin @end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `my/escape-quotes'"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))

(defun my-kill-whole-line (&optional arg)
  "Kill the current line but preserve the column position."
  (interactive "p")
  (save-column
   (kill-whole-line arg)))

;; Source: https://stackoverflow.com/questions/4987760/how-to-change-size-of-split-screen-emacs-windows/4988206
(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

;; Move line
;; Source: https://www.emacswiki.org/emacs/MoveLine
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

;; Duplicate line
(defun duplicate-line()
  (interactive)
  (save-column
   (move-beginning-of-line 1)
   (kill-line)
   (yank)
   (open-line 1)
   (next-line 1)
   (yank)))

;; Switch focus to minibuffer window.
;; Source: https://superuser.com/questions/132225/how-to-get-back-to-an-active-minibuffer-prompt-in-emacs-without-the-mouse
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (when (active-minibuffer-window)
    (select-frame-set-input-focus (window-frame (active-minibuffer-window)))
    (select-window (active-minibuffer-window))))
(global-set-key (kbd "<f7>") 'switch-to-minibuffer-window)

;; https://emacs.stackexchange.com/questions/41222/how-can-i-pass-the-no-line-break-argument-to-base64-encode-region-in-m-x
(defun my/base64-encode-region-no-break ()
  (interactive)
  (base64-encode-region (mark) (point) t))

(defun base64-encode-region-prefix-arg (&rest _args)
  "Pass prefix arg as third arg to `base64-encode-region'."
  (interactive "r\nP"))
(advice-add 'base64-encode-region :before #'base64-encode-region-prefix-arg)

;; Source: http://ergoemacs.org/emacs/elisp_escape_quotes.html
(defun my/escape-quotes (@begin @end)
  "Replace 「\"」 by 「\\\"」 in current line or text selection.
See also: `my/unescape-quotes'"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\"" nil t)
        (replace-match "\\\"" "FIXEDCASE" "LITERAL")))))

(defun my/unescape-quotes (@begin @end)
  "Replace  「\\\"」 by 「\"」 in current line or text selection.
See also: `my/escape-quotes'"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (save-excursion
    (save-restriction
      (narrow-to-region @begin @end)
      (goto-char (point-min))
      (while (search-forward "\\\"" nil t)
        (replace-match "\"" "FIXEDCASE" "LITERAL")))))

(defun my-kill-whole-line (&optional arg)
  "Kill the current line but preserve the column position."
  (interactive "p")
  (save-column
   (kill-whole-line arg)))

;; Source: https://stackoverflow.com/questions/4987760/how-to-change-size-of-split-screen-emacs-windows/4988206
(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 2)))

;; https://www.emacswiki.org/emacs/WholeLineOrRegion
(defun my-kill-ring-save (beg end flash)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2) 'flash)))
  (kill-ring-save beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
          (goto-char end)
        (goto-char beg))
      (sit-for blink-matching-delay))))
(global-set-key [remap kill-ring-save] 'my-kill-ring-save)

(defun my-kill-region (beg end flash)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end) nil)
                 (list (line-beginning-position)
                       (line-beginning-position 2) 'flash)))
  (kill-region beg end)
  (when flash
    (save-excursion
      (if (equal (current-column) 0)
          (goto-char end)
        (goto-char beg))
      (sit-for blink-matching-delay))))
(global-set-key [remap kill-region] 'my-kill-region)
