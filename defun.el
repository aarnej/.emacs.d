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

(defun me/duplicate-line (&optional stay)
  "Duplicate current line.
With optional argument STAY true, leave point where it was."
  (save-excursion
    (move-end-of-line nil)
    (save-excursion
      (insert (buffer-substring (point-at-bol) (point-at-eol))))
    (newline))
  (unless stay
    (let ((column (current-column)))
      (forward-line)
      (forward-char column))))

(defun me/duplicate-backward ()
  "Duplicate current line upward or region backward.
If region was active, keep it so that the command can be repeated."
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark)
        (save-excursion
          (insert (buffer-substring (region-beginning) (region-end)))))
    (me/duplicate-line t)))

(defun me/duplicate-forward ()
  "Duplicate current line downward or region forward.
If region was active, keep it so that the command can be repeated."
  (interactive)
  (if (region-active-p)
      (let (deactivate-mark (point (point)))
        (insert (buffer-substring (region-beginning) (region-end)))
        (push-mark point))
    (me/duplicate-line)))

(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

