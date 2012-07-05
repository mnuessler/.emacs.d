;; Start Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Initialize smex, a smart Emacs enhancement
(require 'smex)
(smex-initialize)

;; Reuse directory buffers in dired (provided by dired-plus)
;; see http://emacswiki.org/emacs/DiredReuseDirectoryBuffer
(toggle-diredp-find-file-reuse-dir 1)

