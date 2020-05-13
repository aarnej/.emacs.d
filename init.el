;; GNU Emacs 26.1

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'use-package)

(use-package adoc-mode
  :ensure t
  :bind (:map adoc-mode-map ("C-c C-t" . adoc-adjust-title-del))
  :mode (("\\.adoc\\'" . adoc-mode))
  :config
  (add-hook 'adoc-mode-hook
          (lambda ()
            (electric-indent-local-mode 0))))

(use-package xterm-color
  :ensure t
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              (add-hook
               'comint-preoutput-filter-functions
               'xterm-color-filter nil t))))

(use-package spinner
  :load-path "lisp/spinner")

(use-package deadgrep
  :requires spinner
  :load-path "lisp/deadgrep"
  :bind (("\C-cg" . deadgrep)))

;; (use-package ag
;;   :ensure t
;;   :bind (("\C-c \S-g" .
;; 	      (lambda () (interactive)
;; 	        (setq current-prefix-arg '(4)) ; C-u
;; 	        (call-interactively 'ag)))
;;          ("\C-cg" .
;; 	      (lambda () (interactive)
;; 	        (setq current-prefix-arg '(4)) ; C-u
;; 	        (call-interactively 'ag-project-regexp))))
;;   :config
;;   (add-hook 'ag-mode-hook (lambda () (setq truncate-lines t))))

;; (autoload 'wgrep-ag-setup "wgrep-ag")
;; (add-hook 'ag-mode-hook 'wgrep-ag-setup)

(use-package wgrep
  :load-path "lisp/Emacs-wgrep")

;; (use-package wgrep-ag
;;   :load-path "lisp/Emacs-wgrep")

(use-package dired-filter
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (global-flycheck-mode)

  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  (defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)
  )



(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(use-package js2-mode
  :ensure t
  ;;:mode "\\.js\\'"
)

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  )

(use-package web-mode
  :ensure t
  :mode "\\.tsx\\'" "\\.ts\\'" "\\.js\\'"
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (company-mode +1)
    (setq-default web-mode-comment-formats '(("javascript" . "//")
                                             ("jsx" . "//")
                                             ("tsx" . "//")))
    )

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  (add-hook 'web-mode-hook
            (lambda ()
              (when (member (file-name-extension buffer-file-name) '("tsx" "ts"))
                (setup-tide-mode))))
  )

(use-package prettier
  :ensure t
  :after (web-mode)
  :config
  (defun my-prettier-before-save-hook ()
    (when (member major-mode '(rjsx-mode web-mode))
      (prettier-prettify)))

  (add-hook 'before-save-hook #'my-prettier-before-save-hook))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (unbind-key "C-c <left>" markdown-mode-map)
  (unbind-key "C-c <right>" markdown-mode-map)
  (unbind-key "C-c <up>" markdown-mode-map)
  (unbind-key "C-c <down>" markdown-mode-map)
  )

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

(use-package yaml-mode
  :ensure t)

(use-package python-docstring
  :ensure t
  :config
  (python-docstring-install))

(use-package json-mode
  :ensure t)

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1))

(use-package ido
  :ensure t
  :config
  (ido-mode 1)
  ;;(defvar ido-enable-replace-completing-read t
  ;;  "If t, use ido-completing-read instead of completing-read if possible.
  ;;
  ;;  Set it to nil using let in around-advice for functions where the
  ;;  original completing-read is required.  For example, if a function
  ;;  foo absolutely must use the original completing-read, define some
  ;;  advice like this:
  ;;
  ;;  (defadvice foo (around original-completing-read-only activate)
  ;;    (let (ido-enable-replace-completing-read) ad-do-it))")
  ;;
  ;;;; Replace completing-read wherever possible, unless directed otherwise
  ;;(defadvice completing-read
  ;;    (around use-ido-when-possible activate)
  ;;  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
  ;;          (and (boundp 'ido-cur-list)
  ;;               ido-cur-list)) ; Avoid infinite loop from ido calling this
  ;;      ad-do-it
  ;;    (let ((allcomp (all-completions "" collection predicate)))
  ;;      (if allcomp
  ;;          (setq ad-return-value
  ;;                (ido-completing-read prompt
  ;;                                     allcomp
  ;;                                     nil require-match initial-input hist def))
  ;;        ad-do-it)))))
)

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode))

(use-package request
  :ensure t
  )

(use-package frame-fns
  :load-path "lisp/frame-fns")

(use-package frame-cmds
  :load-path "lisp/frame-cmds"
  :bind (("C-x 5 r" . rename-frame)))

(use-package icicles
  :load-path "lisp/icicles"
  :bind (("C-x 5 o" . icicle-select-frame)))

(use-package magit
  :load-path "lisp/magit/lisp"
  :commands (magit-status)
  :bind (("C-x v l" . magit-log-buffer-file)
         ("C-x v =" . magit-diff-buffer-file)
         ("C-x g" . magit-status))
  :config
  (magit-define-popup-switch 'magit-fetch-popup
      ?t "Fetch all tags" "--tags")

  (defface magit-aarne-review-name
    '((((class color) (background light)) :foreground "cyan")
      (((class color) (background  dark)) :foreground "cyan"))
    "Face for review name.")

  (defface magit-aarne-review-status
    '((((class color) (background light)) :foreground "brightmagenta")
      (((class color) (background  dark)) :foreground "brightmagenta"))
    "Face for review status.")

  (defface magit-aarne-review-commit-status
    '((((class color) (background light)) :foreground "yellow")
      (((class color) (background  dark)) :foreground "yellow"))
    "Face for review status.")

  (defun* magit-insert-aarne-gerrit-reviews ()
    (let* ((logs (magit-git-lines "log" "@{upstream}.."))
           (commits (--filter (string-match "Change-Id" it) logs))
           (change-ids (or
                        (--map (substring it 15) commits)
                        (return-from magit-insert-aarne-gerrit-reviews)))
           (query (string-join
                   (--map (concat "change:" it) change-ids) " OR "))
           (req (request
                 "https://gerrit.ericsson.se/a/changes/"
                 :params (list (cons "q" query) '("o" . "ALL_REVISIONS"))
                 :parser (lambda ()
                           (forward-line) ;; skip XSSI prevention magic
                           (json-read))
                 :sync t))
           (data (request-response-data req))
           )

      (magit-insert-section (reviews nil t)
        (magit-insert-heading "Reviews:")
        (mapc (lambda (it)
                (magit-insert-section (review)
                  (insert
                   (propertize
                    (format "%s" (cdr (assoc '_number it))) 'face 'magit-aarne-review-name)
                   " "
                   (propertize
                    (format "%s" (cdr (assoc 'status it))) 'face 'magit-aarne-review-status)
                   " "
                   (propertize
                    (format "%s" (substring
                                  (cdr (assoc 'current_revision it)) 0 9))
                            'face 'magit-aarne-review-status)
                   " "
                   (cdr (assoc 'subject it)) ?\n)))
              data)
        (insert ?\n)
        )
      )
    )

  (magit-add-section-hook 'magit-status-sections-hook
			              'magit-insert-aarne-gerrit-reviews
                          'magit-insert-unpulled-from-pushremote)
  (magit-add-section-hook 'magit-status-sections-hook
			              'magit-insert-ignored-files
                          nil t)
  (magit-add-section-hook 'magit-status-sections-hook
			              'magit-insert-worktrees
                          nil t)
  )

(use-package magit-gerrit
  :load-path "lisp/magit-gerrit"
  :config
  (setq-default magit-gerrit-ssh-creds ""))

(use-package projectile
  :ensure t
  :bind ("C-x f" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1))

(use-package recentf
    :ensure t)

(use-package robot-mode
  :load-path "lisp/robot-mode"
  :mode "\\.robot\\'")

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package whitespace
  :ensure t
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'makefile-gmake-mode-hook #'whitespace-mode)
  )

(use-package eslint-fix
  :ensure t)

;;(global-unset-key "\C-x\C-c")

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(global-set-key (kbd "C-x C-b") 'ibuffer)

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

(define-key dired-mode-map "Q" 'dired-do-query-replace-regexp)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (delete 'indentation::space (symbol-value 'whitespace-style))))

(load-file "~/.emacs.d/defun.el")
(global-set-key (kbd "C-c s")  'yank-isearch-string)
(add-hook 'find-file-hook 'commit_msg_hook)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'magit-clean 'disabled nil)
