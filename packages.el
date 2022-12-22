(use-package idomenu)

;; (use-package xterm-color
;;   :config
;;   (setq comint-output-filter-functions
;;         (remove 'ansi-color-process-output comint-output-filter-functions))
;;   (add-hook 'shell-mode-hook
;;             (lambda ()
;;               (add-hook
;;                'comint-preoutput-filter-functions
;;                'xterm-color-filter nil t))))

;; (use-package spinner)

(use-package pyvenv)

(use-package rg
  :custom
  (rg-command-line-flags '("--max-columns" "240" "--max-columns-preview"))
  :config
  ;; (rg-enable-menu "\C-cr")
  (defadvice rg-run (after rg-run-after activate)
    (rg-save-search))
)

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  :config
  (set-face-attribute 'wgrep-face nil :background "gray80"))

(use-package smart-mode-line
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'dark)
  :config
  (sml/setup))

;; (use-package company
;;   :hook ((emacs-lisp-mode . company-mode)
;;          (python-mode . company-mode))
;;   :custom
;;   (company-tooltip-align-annotations t)
;;   (company-idle-delay 0.2))

(use-package dired-filter)

(use-package flycheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-flake8rc ".flake8")
  (flycheck-shellcheck-follow-sources nil)
  (flycheck-disabled-checkers '(javascript-jshint emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode)
  ;; (defvar flycheck-python-flake8-executable "/home/aarne/.pyenv/shims/python")
)

;; (use-package rjsx-mode
;;   :mode "\\.js\\'")

;; (use-package js2-mode)

(use-package dash
  :config
  (with-eval-after-load 'info-look (dash-register-info-lookup)))

(use-package restclient
  :custom
  (restclient-same-buffer-response nil)
  (restclient-response-body-only t)
  (restclient-content-type-modes '(("text/xml" . xml-mode)
                                   ("text/plain" . text-mode)
                                   ("application/xml" . xml-mode)
                                   ("application/json" . json-mode)
                                   ("image/png" . image-mode)
                                   ("image/jpeg" . image-mode)
                                   ("image/jpg" . image-mode)
                                   ("image/gif" . image-mode)
                                   ("text/html" . html-mode))))

(use-package tree-sitter
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook
            (lambda ()
              (tree-sitter-hl-mode)
              (set-face-attribute 'tree-sitter-hl-face:property nil :slant 'normal))))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package tsi
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

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

;; (use-package pyenv-mode
;;   :straight (pyenv-mode :type git :host github :repo "aarnej/pyenv-mode")
;;   :config
;;   (defun ssbb-pyenv-hook ()
;;     "Automatically activates pyenv version if .python-version file exists."
;;     (f-traverse-upwards
;;      (lambda (path)
;;        (let ((pyenv-version-path (f-expand ".python-version" path)))
;;          (when (f-exists? pyenv-version-path)
;;            (let ((version (s-trim (f-read-text pyenv-version-path 'utf-8))))
;;           (pyenv-mode t)
;;              (pyenv-mode-set version)
;;              (setq-local pyvenv-activate (pyenv-mode-full-path version)
;;              )
;;            ))))))

;;   (add-hook 'python-mode-hook 'ssbb-pyenv-hook)
;;   (pyvenv-tracking-mode))

;; (use-package ace-jump-mode)

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
  (magit-delete-by-moving-to-trash nil)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (magit-refs-sections-hook '(magit-insert-error-header
                              magit-insert-branch-description
                              magit-insert-local-branches
                              magit-insert-remote-branches))
  (magit-diff-extra-stat-arguments '("--stat-width=200"))
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
  :custom
  (projectile-switch-project-action #'projectile-vc)
  (projectile-indexing-method 'alien)
  :config
  (projectile-mode +1)
  (diminish 'projectile-mode))

(use-package recentf)

(use-package undo-tree
  :custom
  (undo-tree-auto-save-history nil)
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

;; (use-package popper
;;   ;; :bind (("C-`"   . popper-toggle-latest)
;;   ;;        ("M-`"   . popper-cycle)
;;   ;;        ("C-M-`" . popper-toggle-type))
;;   :custom
;;   (popper-reference-buffers
;;    '("\\*Messages\\*"
;;      "Output\\*$"
;;      "\\*Async Shell Command\\*"
;;      help-mode
;;      compilation-mode
;;      rg-mode))
;;   (popper-mode +1)
;;   (popper-echo-mode +1))                ; For echo area hints

(use-package ace-window
  :custom
  (aw-leading-char-style 'path)
  (aw-display-mode-overlay nil)
  (aw-background nil)
  :config
  (ace-window-display-mode))

(use-package clojure-mode)
