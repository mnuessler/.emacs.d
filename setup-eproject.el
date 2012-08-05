(require 'eproject)
(require 'eproject-extras)

(define-project-type java (generic)
  (or (look-for ".project") (look-for ".classpath"))
  :relevant-files ("\\.java$" "\\.properties$" "\\.xml$" "\\.xsl" "\\.md$" "\\.txt$" "\\.fml$")
  :irrelevant-files ("target"))

;;(define-project-type java-maven (java)
;;  (look-for "pom.xml")
;;  :relevant-files ("\\.fml$"))

(provide 'setup-eproject)
