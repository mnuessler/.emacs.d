(defun bobby-schema-version (prefix version)
  "Replaces all database schema references in buffer with the versioned schema name."
  (interactive "P\nnTarget schema version: ")
  (defun replace-all (search-str replacement)
    (save-excursion
      (goto-char 1)
      (while (re-search-forward search-str nil t)
	(replace-match replacement))))
  (let*
      ((ver-str (number-to-string version))
       (api (if prefix "web" "admin"))
       (rep-admin (concat "bobby_admin_api_v" ver-str))
       (rep-web (concat "bobby_web_api_v" ver-str))
       (rep-api (concat "bobby_" api "_api_v" ver-str))
       (save-excursion
	 (progn
	   (goto-char 1)
	   (while (re-search-forward "bobby_web_api" nil t)
	     (replace-match rep-web))
	   (goto-char 1)
	   (while (re-search-forward "bobby_admin_api" nil t)
	     (replace-match rep-admin))
	   (goto-char 1)
	   (while (re-search-forward "{bobby_api}" nil t)
	     (replace-match rep-api)))))))
