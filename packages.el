(use-package idomenu)

(use-package xterm-color
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              (add-hook
               'comint-preoutput-filter-functions
               'xterm-color-filter nil t))))

(use-package spinner)

(use-package pyvenv)

(use-package rg
  :custom
  (rg-command-line-flags '("--max-columns" "240" "--max-columns-preview"))
  :config
  (rg-define-search rg-aarne-dir :files "*" :dir current)
  (rg-define-search rg-aarne :files "*" :dir project))

(use-package wgrep)

(use-package smart-mode-line
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'dark)
  :config
  (sml/setup))

(use-package company
  :hook ((emacs-lisp-mode . company-mode)
         (python-mode . company-mode)
         (web-mode . company-mode))
  :custom
  (company-tooltip-align-annotations t)
  (company-idle-delay nil))

(use-package dired-filter)

(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-flake8rc ".flake8")
  (flycheck-shellcheck-follow-sources nil)
  (flycheck-disabled-checkers '(javascript-jshint emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode)
  (defvar flycheck-python-flake8-executable "/home/aarne/.pyenv/shims/python"))

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package js2-mode)

(use-package dash
  :config
  (with-eval-after-load 'info-look (dash-register-info-lookup)))

;; (use-package lsp-mode
;;   :after (which-key)
;;   :hook ((web-mode . lsp))
;;   :custom
;;   (lsp-keymap-prefix "C-c l")
;;   (lsp-auto-guess-root nil)
;;   (lsp-eslint-server-command '("node"
;; 	                           "/home/aarne/repos/vscode-eslint/server/out/eslintServer.js"
;; 	                           "--stdio"))
;;   :config
;;   (add-hook 'lsp-after-initialize-hook (lambda () (flycheck-add-next-checker 'lsp 'python-flake8)))
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; (use-package lsp-python-ms
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook
;;   (python-mode . (lambda ()
;;                    (require 'lsp-python-ms)
;;                    (lsp-deferred))))

(use-package restclient)

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode))
  :custom
  ;; aligns annotation to the right hand side
  (web-mode-code-indent-offset 2)
  (web-mode-comment-formats '(("javascript" . "//")
                              ("jsx" . "//")
                              ("tsx" . "//")
                              ("typescript" . "//")))
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (member (file-name-extension buffer-file-name) '("tsx" "ts"))
                ;; To make comment filling work, copied these lines from
                ;; typescript-mode.el
                (setq c-comment-prefix-regexp "//+\\|\\**"
                      c-paragraph-start "$"
                      c-paragraph-separate "$"
                      c-block-comment-prefix "* "
                      c-line-comment-starter "//"
                      c-comment-start-regexp "/[*/]\\|\\s!"
                      comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

                (setq-local electric-indent-chars
                            (append "{}():;," electric-indent-chars))
                (setq-local electric-layout-rules
                            '((?\; . after) (?\{ . after) (?\} . before)))

                (let ((c-buffer-is-cc-mode t))
                  ;; FIXME: These are normally set by `c-basic-common-init'.  Should
                  ;; we call it instead?  (Bug#6071)
                  (make-local-variable 'paragraph-start)
                  (make-local-variable 'paragraph-separate)
                  (make-local-variable 'paragraph-ignore-fill-prefix)
                  (make-local-variable 'adaptive-fill-mode)
                  (make-local-variable 'adaptive-fill-regexp)
                  (c-setup-paragraph-variables))

                (setq indent-tabs-mode nil)
		(setq fill-paragraph-function 'c-fill-paragraph)))))

(use-package nvm)

(use-package iter2)

;; (use-package prettier
;;   :straight (prettier :type git :host github :repo "jscheid/prettier.el")
;;   :after (web-mode)
;;   :config
;;   (defun my-prettier-before-save-hook ()
;;     (when (member major-mode '(rjsx-mode web-mode typescript-mode))
;;       (prettier-prettify)))
;;   (add-hook 'before-save-hook #'my-prettier-before-save-hook))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "multimarkdown")
  (markdown-asymmetric-header t)
  :config
  (add-hook 'markdown-mode-hook (lambda () (setq indent-tabs-mode nil))))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package yaml-mode
  :custom
  (yaml-block-literal-electric-alist '((124 . "") (62 . ""))))

(use-package python-docstring
  :custom
  (python-docstring-sentence-end-double-space nil)
  :config
  (python-docstring-install))

(use-package json-mode)

(use-package pyenv-mode
  :straight (pyenv-mode :type git :host github :repo "aarnej/pyenv-mode")
  :config
  (defun ssbb-pyenv-hook ()
    "Automatically activates pyenv version if .python-version file exists."
    (f-traverse-upwards
     (lambda (path)
       (let ((pyenv-version-path (f-expand ".python-version" path)))
         (when (f-exists? pyenv-version-path)
           (let ((version (s-trim (f-read-text pyenv-version-path 'utf-8))))
	     (pyenv-mode t)
             (pyenv-mode-set version)
             (setq-local pyvenv-activate (pyenv-mode-full-path version)
             )
           ))))))

  (add-hook 'python-mode-hook 'ssbb-pyenv-hook)
  (pyvenv-tracking-mode))

(use-package ace-jump-mode)

(use-package expand-region)

(use-package multiple-cursors)

(use-package flx-ido
  :custom
  (flx-ido-threshold 500)
  :config
  (flx-ido-mode 1))

(use-package ido-completing-read+
  :after (ido)
  :config
  (ido-ubiquitous-mode 1))

(use-package ido
  :custom
  (ido-auto-merge-work-directories-length -1)
  :config
  (add-to-list 'ido-read-file-name-non-ido 'dired-create-directory)
  (ido-mode 1)
  (ido-everywhere 1)
)

(use-package diminish
  :config
  (diminish 'auto-revert-mode))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode))

(use-package request
  :custom
  (request-curl-options '("--netrc")))

(use-package magit
  :straight (magit :type git :host github :repo "aarnej/magit")
  :commands (magit-status)
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-fetch-arguments '("--tags"))
  (magit-log-arguments '("-n256" "--graph" "--decorate"))
  (magit-log-margin-spec '(28 1 magit-duration-spec))
  (magit-log-section-arguments '("-n256"))
  (magit-log-section-commit-count 20)
  (magit-log-show-margin nil)
  (magit-reflog-show-margin nil)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-refs-sections-hook '(magit-insert-error-header
                              magit-insert-branch-description
                              magit-insert-local-branches
                              magit-insert-remote-branches))

  :config
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" "--tags"))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpulled-from-pushremote)
  (magit-add-section-hook 'magit-status-sections-hook
			              'magit-insert-ignored-files
                          nil t)
  (magit-add-section-hook 'magit-status-sections-hook
			              'magit-insert-worktrees
                          nil t)
  )

(use-package projectile
  :config
  (projectile-mode +1)
  (diminish 'projectile-mode))

(use-package recentf)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (diminish 'undo-tree-mode))

(use-package whitespace
  :custom
  (whitespace-global-modes '(python-mode))
  (whitespace-line-column 79)
  (whitespace-style '(face trailing tab-mark))
  :config
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'makefile-gmake-mode-hook #'whitespace-mode)
  (diminish 'global-whitespace-mode)
  )

(use-package which-key
  :custom
  (which-key-idle-delay 2.0)
  :config
  (which-key-mode))

(use-package eslint-fix)

(use-package vlf
  :config
  (require 'vlf-setup))

(use-package move-text)

(use-package dockerfile-mode)

;; (use-package shackle
;;   :hook
;;   (after-init . shackle-mode)
;;   :custom
;;   (shackle-inhibit-window-quit-on-same-windows t)
;;   (shackle-rules '((help-mode :same t)
;;                    (helpful-mode :same t)
;;                    (process-menu-mode :same t)))
;;   (shackle-select-reused-windows t))

(use-package popper
  ;; :bind (("C-`"   . popper-toggle-latest)
  ;;        ("M-`"   . popper-cycle)
  ;;        ("C-M-`" . popper-toggle-type))
  :custom
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     compilation-mode
     rg-mode
     magit-status-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints
