;; Start Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Initialize smex, a smart Emacs enhancement
(require 'smex)
(smex-initialize)

