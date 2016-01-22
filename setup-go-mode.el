;;(require 'go-mode)

(setenv "GOPATH" "/home/matthias.nuessler/projects/go")
(setenv "PATH" (concat (getenv "PATH") ":" (getenv "GOPATH") "/bin"))

(add-to-list 'exec-path "/home/matthias.nuessler/projects/go/bin")

(add-hook 'before-save-hook 'gofmt-before-save)

;;(load-file "/home/matthias.nuessler/projects/go/src/golang.org/x/tools/cmd/oracle/oracle.el")

;;(require 'go-autocomplete)
;;(require 'auto-complete-config)
;;(ac-config-default)

(provide 'setup-go-mode)
