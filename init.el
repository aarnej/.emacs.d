;; GNU Emacs 26.1 (build 2, x86_64-pc-linux-gnu, GTK+ Version 3.18.9)
 of 2018-05-29

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
                          '("elpy" . "https://jorgenschaefer.github.io/packages/"))
(package-initialize)

(global-undo-tree-mode)
(elpy-enable)

;;(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'xcscope)
(require 'whitespace)
(require 'ag)
(require 'recentf)
(require 'cc-mode)
(require 'browse-kill-ring)


(load-file "~/repo/magit-gerrit/magit-gerrit.el")
;; (require 'magit-gerrit)
(setq-default magit-gerrit-ssh-creds "")


(defun yank-isearch-string ()
  (interactive)
  (insert (symbol-value 'isearch-string)))

(load-file "~/repo/robot-mode/robot-mode.el")
(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))


(global-set-key (kbd "C-c s")  'yank-isearch-string)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (delete 'indentation::space (symbol-value 'whitespace-style))))

(add-hook 'before-save-hook 'whitespace-cleanup)

(browse-kill-ring-default-keybindings)

(global-unset-key "\C-x\C-c")

(global-set-key (kbd "C-x f") 'fiplr-find-file)

(global-set-key (kbd "C-x v l") 'magit-log-buffer-file)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; to get CTRL + arrow keys working
;;(define-key input-decode-map "\e[1;3A" [C-up])
;;(define-key input-decode-map "\e[1;5A" [C-up])
;;
;;(define-key input-decode-map "\e[1;3B" [C-down])
;;(define-key input-decode-map "\e[1;5B" [C-down])
;;
;;(define-key input-decode-map "\e[1;3C" [C-right])
;;(define-key input-decode-map "\e[1;5C" [C-right])
;;
;;(define-key input-decode-map "\e[1;3D" [C-left])
;;(define-key input-decode-map "\e[1;5D" [C-left])

(define-key input-decode-map "\e[A" [C-up])
(define-key input-decode-map "\e[B" [C-down])
(define-key input-decode-map "\e[C" [C-right])
(define-key input-decode-map "\e[D" [C-left])

(add-to-list 'auto-mode-alist '("\\.ddl\\'" . c-mode))
(add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))

(font-lock-add-keywords
 'c-mode
  '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

;; imenu
(define-key c-mode-map "\C-xj" 'imenu)
(define-key c-mode-map "\C-xp" 'linum-mode)

;; enriched mode RET = newline-and-indent
(require 'enriched)
(define-key enriched-mode-map [remap newline-and-indent] nil)
(define-key enriched-mode-map [remap move-beginning-of-line] nil)

(defun commit_msg_hook ()
  (when (and (stringp buffer-file-name)
             (string-match "\\COMMIT_EDITMSG\\'" buffer-file-name))
    (setq fill-column 65)))

(add-hook 'find-file-hook 'commit_msg_hook)

(global-set-key "\C-cg"
                (lambda () (interactive)
                  (setq current-prefix-arg '(4)) ; C-u
                  (call-interactively 'ag)))

(cscope-setup)
(add-hook 'python-mode-hook (function cscope-minor-mode))

;(load-file "github/robot-mode/robot-mode.el")
;(add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode))

(defun vc-next-change ()
  (interactive)
  (re-search-forward "^ .*\n[^ ]" nil t)
  )
(defun vc-prev-change ()
  (interactive)
  (re-search-backward "^ .*\n[^ ]" nil t)
  )

(require 'diff-mode)
(define-key diff-mode-map "\C-n" 'vc-next-change)
(define-key diff-mode-map "\C-p" 'vc-prev-change)


(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--follow --date=iso" "-C" "-C" rev "--" name)))

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

(defun cscope-process-filter (process output)
  "Accept cscope process output and reformat it for human readability.
Magic text properties are added to allow the user to select lines
using the mouse."
  (let ( (old-buffer (current-buffer)) )
    (with-current-buffer (process-buffer process)
      (let (line file function-name line-number)
        (save-excursion
          (goto-char cscope-last-output-point)
          ;; Get the output thus far ...
          (if cscope-process-output
              (setq cscope-process-output (concat cscope-process-output
                                                  output))
            (setq cscope-process-output output))
          ;; Slice and dice it into lines.
          ;; While there are whole lines left ...
          (while (and cscope-process-output
                      (string-match "\\([^\n]+\n\\)\\(\\(.\\|\n\\)*\\)"
                                    cscope-process-output))
            (setq file				nil
                  glimpse-stripped-directory	nil
                  )
            ;; Get a line
            (setq line (substring cscope-process-output
                                  (match-beginning 1) (match-end 1)))
            (setq cscope-process-output (substring cscope-process-output
                                                   (match-beginning 2)
                                                   (match-end 2)))
            (if (= (length cscope-process-output) 0)
                (setq cscope-process-output nil))

            ;; This should always match.
            (if (string-match
                 "^\\([^ \t]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)\n"
                 line)
                (progn
                  (let (str)
                    (setq file (substring line (match-beginning 1)
                                          (match-end 1))
                          function-name (substring line (match-beginning 2)
                                                   (match-end 2))
                          line-number (substring line (match-beginning 3)
                                                 (match-end 3))
                          line (substring line (match-beginning 4)
                                          (match-end 4))
                          )
                    ;; If the current file is not the same as the previous
                    ;; one ...
                    (if (not (and cscope-last-file
                                  (string= file cscope-last-file)))
                        (progn
                          ;; The current file is different.

                          ;; Insert a separating blank line if
                          ;; necessary.
                          (if cscope-last-file (insert "\n"))
                          ;; Insert the file name
                          (setq str (concat "*** " file ":"))
                          (if cscope-use-face
                              (put-text-property 0 (length str)
                                                 'face 'cscope-file-face
                                                 str))
                          (cscope-insert-with-text-properties
                           str
                           (expand-file-name file))
                          (insert "\n")))

                    (if cscope-first-match-point
                        (setq cscope-matched-multiple t)
                      (setq cscope-first-match-point (point)))

                    ;; ... and insert the line, with the
                    ;; appropriate indentation.
                    (cscope-insert-with-text-properties
                     (cscope-make-entry-line function-name
                                             line-number
                                             line)
                     (expand-file-name file)
                     line-number
                     line)
                    (insert "\n")
                    (setq cscope-last-file file)
                    ))
              ;;(insert line "\n")
              ))
          (setq cscope-last-output-point (point)))
        (set-buffer-modified-p nil)))))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 60)
 '(ag-arguments
   (quote
    ("--ignore-case" "--stats" "--ignore" "*.html" "--ignore" "*.xml" "--ignore" "report-mgr-ref-reports.txt" "--ignore" "bundle-*.js" "--ignore" "bundle-*.js.map")))
 '(column-number-mode t)
 '(confirm-nonexistent-file-or-buffer t)
 '(cscope-allow-arrow-overlays nil)
 '(cscope-edit-single-match nil)
 '(cscope-indexer-ignored-directories
   (quote
    ("CVS" "RCS" "SCCS" ".git" ".hg" ".bzr" ".cdv" ".pc" ".svn" "_MTN" "_darcs" "_sgbak" "debian" "unittest" "mock" "ut" "test")))
 '(cscope-indexer-suffixes
   (quote
    ("*.[chly]" "*.[ch]xx" "*.[ch]pp" "*.cc" "*.hh" "*.ddl")))
 '(cscope-option-do-not-update-database t)
 '(elpy-rpc-backend "jedi")
 '(elpy-shell-echo-input-cont-prompt nil)
 '(fill-column 79)
 '(fiplr-ignored-globs
   (quote
    ((directories
      (".git" ".svn" ".hg" ".bzr" "node_modules"))
     (files
      (".#*" "*~" "*.so" "*.jpg" "*.png" "*.gif" "*.pdf" "*.gz" "*.zip")))))
 '(git-commit-fill-column 70)
 '(global-whitespace-mode t)
 '(grep-command "grep -nrH -e ")
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(kill-ring-max 1000)
 '(magit-diff-refine-hunk (quote all))
 '(magit-gerrit-push-review-to-topic nil)
 '(magit-log-arguments (quote ("-n256" "--graph" "--decorate")))
 '(magit-log-margin-spec (quote (28 1 magit-duration-spec)))
 '(magit-log-section-arguments (quote ("-n256")))
 '(magit-log-show-margin nil)
 '(magit-reflog-show-margin nil)
 '(magit-refs-sections-hook
   (quote
    (magit-insert-error-header magit-insert-branch-description magit-insert-local-branches magit-insert-remote-branches)))
 '(magit-status-sections-hook
   (quote
    (magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpushed-to-pushremote magit-insert-unpushed-to-upstream-or-recent magit-insert-unpulled-from-pushremote magit-insert-unpulled-from-upstream magit-insert-ignored-files)))
 '(make-backup-files nil)
 '(mode-line-format
   (quote
    ("" mode-line-client mode-line-modified mode-line-frame-identification mode-line-buffer-identification " " mode-line-misc-info mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-remote mode-line-end-spaces)))
 '(mode-require-final-newline nil)
 '(package-selected-packages
   (quote
    (magit-gerrit magit magit-svn xcscope undo-tree js2-mode fiplr elpy browse-kill-ring ag adoc-mode)))
 '(python-fill-docstring-style (quote django))
 '(python-shell-interpreter "python3")
 '(recentf-max-saved-items 1000)
 '(recentf-mode t)
 '(require-final-newline nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(tab-width 4)
 '(truncate-partial-width-windows nil)
 '(vc-display-status nil)
 '(vc-git-diff-switches "--unified=100000")
 '(which-function-mode t)
 '(whitespace-global-modes (quote (python-mode)))
 '(whitespace-line-column 79)
 '(whitespace-style (quote (face trailing lines-tail tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((t (:inherit error :background "color-225" :foreground "brightred"))))
 '(git-rebase-hash ((t (:foreground "gre"))))
 '(highlight-indentation-face ((t (:inherit ##))))
 '(markup-code-face ((t (:inherit (fixed-pitch markup-gen-face) :foreground "color-105"))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :weight bold :height 3.0))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :weight bold :height 2.4))))
 '(markup-title-3-face ((t (:inherit markup-gen-face :height 1.4)))))
(put 'scroll-left 'disabled nil)
