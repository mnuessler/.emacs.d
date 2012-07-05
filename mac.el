;; Extend path on Mac 
(when (equal system-type 'darwin)
  (setenv "PATH" (concat "~/bin:/sw/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

(add-to-list 'exec-path "/sw/bin")

(provide 'mac)

