(defun yank-isearch-string ()
  (interactive)
  (insert (symbol-value 'isearch-string)))

(defun commit_msg_hook ()
  (when (and (stringp buffer-file-name)
	     (string-match "\\COMMIT_EDITMSG\\'" buffer-file-name))
    (setq fill-column 65)))

(defun kill-buffers-with-paths-matching-without-asking (regexp)
  "Kill buffers whose filename matches the specified REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: \n")
  (dolist (buffer (buffer-list))
    (let ((file-name (buffer-file-name buffer)))
      (when (and file-name (not (string-equal file-name ""))
		 (string-match regexp file-name))
	(message "deleted buffer with filename %s" file-name)
	(kill-buffer buffer)))))

(defun kill-matching-buffers-without-asking (regexp)
  "Kill buffers whose name matches the specified REGEXP without asking for confirmation."
  (interactive "sKill buffers matching this regular expression: \n")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
		 (string-match regexp name))
	(message "deleted buffer with name %s" name)
	(kill-buffer buffer)))))
