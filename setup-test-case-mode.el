(autoload 'test-case-mode "test-case-mode" nil t)
(autoload 'enable-test-case-mode-if-test "test-case-mode")
(autoload 'test-case-find-all-tests "test-case-mode" nil t)
(autoload 'test-case-compilation-finish-run-all "test-case-mode")

;; enable it automatically when opening test files
(add-hook 'find-file-hook 'enable-test-case-mode-if-test)

;; run all visited tests after a compilation
(add-hook 'compilation-finish-functions 'test-case-compilation-finish-run-all)

(provide 'setup-test-case-mode)
