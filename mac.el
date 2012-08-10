;; Extend path on Mac 
(when (equal system-type 'darwin)
  (setenv "PATH" (concat "~/bin:/sw/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

(add-to-list 'exec-path "/sw/bin")

;; trash directory (only used when delete-by-moving-to-trash is set to non-nil)
(setq trash-directory "~/.Trash")

(provide 'mac)
