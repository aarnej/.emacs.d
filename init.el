;;; init.el --- the init file
;;; Commentary:
;;

;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Customize Emacs built-in stuff

(load-file "~/.emacs.d/defun.el")
(load-file "~/.emacs.d/vars.el")
(load-file "~/.emacs.d/faces.el")
(load-theme 'tango-dark)

(menu-bar-mode 0)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(global-whitespace-mode 1)
(global-visual-line-mode 1)
(recentf-mode 1)
(show-paren-mode 1)
(winner-mode 1)
(tool-bar-mode 0)
(if (and (fboundp 'server-running-p)
	 (not (server-running-p)))
    (server-start))
(electric-indent-mode 0)

(put 'magit-clean 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(define-key input-decode-map "\e[A" [C-up])
(define-key input-decode-map "\e[B" [C-down])
(define-key input-decode-map "\e[C" [C-right])
(define-key input-decode-map "\e[D" [C-left])

(add-hook 'emacs-lisp-mode-hook
	      (lambda () (delete 'indentation::space (symbol-value 'whitespace-style))))
(add-hook 'find-file-hook 'commit_msg_hook)
(add-hook 'dired-mode-hook
          (lambda () (define-key dired-mode-map "Q" 'dired-do-query-replace-regexp)))
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode 0)))


;; Extra packages

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
  :config
  (setq rg-command-line-flags '("--max-columns" "240" "--max-columns-preview"))
  (rg-define-search rg-aarne
    :files "*"
    :dir project))

(use-package wgrep)

(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (sml/setup))

(use-package company
  :hook ((emacs-lisp-mode . company-mode)
         (python-mode . company-mode)
         (web-mode . company-mode))
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay nil))

(use-package dired-filter)

(use-package flycheck
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-flake8rc ".flake8")
  (setq flycheck-shellcheck-follow-sources nil)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (global-flycheck-mode)
  (defvar flycheck-python-flake8-executable "/home/aarne/.pyenv/shims/python"))

(use-package rjsx-mode
  :mode "\\.js\\'")

(use-package js2-mode)   ;;:mode "\\.js\\'"

(use-package lsp-mode
  :after (which-key)
  :hook (;; (typescript-mode . lsp)
         (web-mode . lsp))
  :config
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-auto-guess-root nil)
  (setq lsp-eslint-server-command
	'("node"
	  "/home/aarne/repos/vscode-eslint/server/out/eslintServer.js"
	  "--stdio"))
  (add-hook 'lsp-after-initialize-hook (lambda
					 ()
					 (flycheck-add-next-checker 'lsp 'python-flake8)))
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   (lsp-deferred))))

(use-package restclient)

(use-package web-mode
  :mode (("\\.tsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode))
  :config
  ;; aligns annotation to the right hand side
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-comment-formats '(("javascript" . "//")
                                   ("jsx" . "//")
                                   ("tsx" . "//")
                                   ("typescript" . "//")))
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)

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
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-asymmetric-header t)
  (add-hook 'markdown-mode-hook (lambda () (setq indent-tabs-mode nil))))

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))

(use-package yaml-mode
  :config
  (setq yaml-block-literal-electric-alist '((124 . "") (62 . ""))))

(use-package python-docstring
  :config
  (setq python-docstring-sentence-end-double-space nil)
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
  :config
  (setq flx-ido-threshold 500)
  (flx-ido-mode 1))

(use-package ido-completing-read+
  :after (ido)
  :config
  (ido-ubiquitous-mode 1))

(use-package ido
  :config
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
  :config
  (setq request-curl-options '("--netrc")))

(use-package magit
  :straight (magit :type git :host github :repo "aarnej/magit")
  :commands (magit-status)
  :config
  (setq magit-diff-refine-hunk 'all)
  (setq magit-fetch-arguments '("--tags"))
  (setq magit-gerrit-push-review-to-topic nil)
  (setq magit-log-arguments '("-n256" "--graph" "--decorate"))
  (setq magit-log-margin-spec '(28 1 magit-duration-spec))
  (setq magit-log-section-arguments '("-n256"))
  (setq magit-log-section-commit-count 20)
  (setq magit-log-show-margin nil)
  (setq magit-reflog-show-margin nil)
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  (setq magit-refs-sections-hook '(magit-insert-error-header
                                   magit-insert-branch-description
                                   magit-insert-local-branches
                                   magit-insert-remote-branches))

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
  (setq projectile-mode-line '(:eval (format " Pj[%s]" (projectile-project-name))))
  (diminish 'projectile-mode))

(use-package recentf)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-mode-lighter " Undo-T")
  (diminish 'undo-tree-mode))

(use-package whitespace
  :config
  (setq whitespace-global-modes '(python-mode))
  (setq whitespace-line-column 79)
  (setq whitespace-style '(face trailing tab-mark))

  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'makefile-gmake-mode-hook #'whitespace-mode)
  (diminish 'global-whitespace-mode)
  )

(use-package which-key
  :config
  (setq which-key-idle-delay 2.0)
  (which-key-mode))

(use-package eslint-fix)

(use-package vlf
  :config
  (require 'vlf-setup))

(use-package move-text)

(use-package dockerfile-mode)

(setq aj-mode-map (make-sparse-keymap))

(define-key aj-mode-map         (kbd "M-<up>")      'move-text-up)
(define-key aj-mode-map         (kbd "M-<down>")    'move-text-down)
;;                                   "C-c C-  reserved by web-mode
(define-key aj-mode-map         (kbd "C-c SPC")     'ace-jump-mode)
(define-key aj-mode-map         (kbd "C-c .")       'company-complete)
(define-key aj-mode-map         (kbd "C-c <down>")  'windmove-down)
(define-key aj-mode-map         (kbd "C-c <left>")  'windmove-left)
(define-key aj-mode-map         (kbd "C-c <right>") 'windmove-right)
(define-key aj-mode-map         (kbd "C-c <up>")    'windmove-up)
(define-key aj-mode-map         (kbd "C-c b n")     'next-buffer)
(define-key aj-mode-map         (kbd "C-c b p")     'previous-buffer)
(define-key aj-mode-map         (kbd "C-c x")       'er/expand-region)
(define-key aj-mode-map         (kbd "C-c g")       'rg-aarne)
(define-key lsp-mode-map        (kbd "C-c l")       lsp-command-map)
(define-key aj-mode-map         (kbd "C-c ma")      'mc/vertical-align-with-space)
(define-key aj-mode-map         (kbd "C-c me")      'mc/edit-lines)
(define-key aj-mode-map         (kbd "C-c mx")      'mc/mark-more-like-this-extended)
(define-key projectile-mode-map (kbd "C-c p")       projectile-command-map)
(define-key aj-mode-map         (kbd "C-c s")       'yank-isearch-string)
(define-key aj-mode-map         (kbd "C-x C-b")     'ibuffer)
(define-key projectile-mode-map (kbd "C-x f")       'projectile-find-file)
(define-key aj-mode-map         (kbd "C-x g")       'magit-status)
(define-key aj-mode-map         (kbd "C-x v l")     'magit-log-buffer-file)
(define-key aj-mode-map         (kbd "C-x v =")     'magit-diff-buffer-file)

(define-minor-mode aj-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t)

(aj-mode 1)

(provide 'init)
