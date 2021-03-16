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

(use-package adoc-mode
  :straight t
  :bind (:map adoc-mode-map ("C-c C-t" . adoc-adjust-title-del))
  :mode (("\\.adoc\\'" . adoc-mode))
  :config
  (add-hook 'adoc-mode-hook
          (lambda ()
            (electric-indent-local-mode 0))))

(use-package xterm-color
  :straight t
  :config
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              (add-hook
               'comint-preoutput-filter-functions
               'xterm-color-filter nil t))))

(use-package spinner
  :straight t)

(use-package pyvenv
  :straight t)

(use-package rg
  :straight t
  :bind (("\C-cg" . rg-aarne))
  :config
  (setq rg-command-line-flags '("--max-columns" "240" "--max-columns-preview"))
  (rg-define-search rg-aarne
    :files "*"
    :dir project))

;; (use-package deadgrep
;;   :requires spinner
;;   :straight (deadgrep :type git :host github :repo "aarnej/deadgrep")
;;   :bind (("\C-cg" . deadgrep)))

;; (use-package ag
;;   :straight t
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
  :straight t)

(use-package smart-mode-line
  :straight t
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'dark)
  (sml/setup))

(use-package company
  :straight t
  :bind (("\C-c." . company-complete))
  :hook ((emacs-lisp-mode . company-mode)
         (python-mode . company-mode)
         (web-mode . company-mode)
         (typescript-mode . company-mode))
  :config
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay nil))

;; for cmake, go to https://apt.kitware.com/
;; install also libtool-bin
;; (use-package vterm
;;   :straight t
;;   :config
;;   (setq vterm-max-scrollback 100000))

;; (use-package wgrep-ag
;;   :straight t)

(use-package dired-filter
  :straight t)

(use-package flycheck
  :straight t
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq flycheck-flake8rc ".flake8")
  (setq flycheck-shellcheck-follow-sources nil)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (global-flycheck-mode)

  ;; (defun my/use-eslint-from-node-modules ()
  ;;   (let* ((root (locate-dominating-file
  ;;                 (or (buffer-file-name) default-directory)
  ;;                 "node_modules"))
  ;;          (eslint (and root
  ;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
  ;;                                         root))))
  ;;     (when (and eslint (file-executable-p eslint))
  ;;       (setq-local flycheck-javascript-eslint-executable eslint))))

  ;; (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  ;; (defvaralias 'flycheck-python-flake8-executable 'python-shell-interpreter)
  (defvar flycheck-python-flake8-executable "/home/aarne/.pyenv/shims/python")
  )

(use-package rjsx-mode
  :straight t
  :mode "\\.js\\'")

(use-package js2-mode
  :straight t)   ;;:mode "\\.js\\'"

;; (use-package tide
;;   :straight t
;;   ;; :after (web-mode typescript-mode company flycheck)
;;   :config
;;   (defun setup-tide-mode ()
;;     (interactive)
;;     (tide-setup)
;;     (flycheck-mode +1)
;;     (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;     (eldoc-mode +1)
;;     (tide-hl-identifier-mode +1)
;;     (company-mode +1)
;;     (setq-default web-mode-comment-formats '(("javascript" . "//")
;;                                              ("typescript" . "//")
;;                                              ("jsx" . "//")
;;                                              ("tsx" . "//"))))
;;   (add-hook 'typescript-mode-hook #'setup-tide-mode)
;;   (add-hook 'web-mode-hook #'setup-tide-mode))

(use-package typescript-mode
  :straight t
  :config
  (setq typescript-indent-level 2))

(use-package lsp-mode
  :straight t
  :after (which-key)
  :hook ((typescript-mode . lsp)
         (web-mode . lsp))
  :bind-keymap
  ("C-c l" . lsp-command-map)
  :config
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
  :straight t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook
  (python-mode . (lambda ()
                   (require 'lsp-python-ms)
                   (lsp-deferred))))

(use-package restclient
  :straight t)

(use-package web-mode
  :straight t
  :mode "\\.tsx\\'"
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

(use-package nvm
  :straight t)

(use-package iter2
  :straight t)

;; (use-package prettier
;;   :straight (prettier :type git :host github :repo "jscheid/prettier.el")
;;   :after (web-mode)
;;   :config
;;   (defun my-prettier-before-save-hook ()
;;     (when (member major-mode '(rjsx-mode web-mode typescript-mode))
;;       (prettier-prettify)))
;;   (add-hook 'before-save-hook #'my-prettier-before-save-hook))

(use-package markdown-mode
  :straight t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-asymmetric-header t)
  (unbind-key "C-c <left>" markdown-mode-map)
  (unbind-key "C-c <right>" markdown-mode-map)
  (unbind-key "C-c <up>" markdown-mode-map)
  (unbind-key "C-c <down>" markdown-mode-map)
  (add-hook 'markdown-mode-hook (lambda () (setq indent-tabs-mode nil))))

(use-package browse-kill-ring
  :straight t
  :config
  (browse-kill-ring-default-keybindings))

(use-package yaml-mode
  :straight t
  :config
  (setq yaml-block-literal-electric-alist '((124 . "") (62 . ""))))

(use-package python-docstring
  :straight t
  :config
  (setq python-docstring-sentence-end-double-space nil)
  (python-docstring-install))

(use-package json-mode
  :straight t)

;; (use-package paredit
;;   :straight t
;;   :config
;;   (add-hook 'python-mode-hook #'enable-paredit-mode)
;;   (add-hook 'web-mode-hook #'enable-paredit-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode))

;; (use-package smartparens
;;   :straight t
;;   :config
;;   (require 'smartparens-config)
;;   (sp-use-smartparens-bindings)
;;   (add-hook 'python-mode-hook #'smartparens-strict-mode)
;;   (add-hook 'typescript-mode-hook #'smartparens-strict-mode)
;;   (add-hook 'web-mode-hook #'smartparens-mode)
;;   (add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
;;   (add-hook 'sh-mode-hook #'smartparens-strict-mode))

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

;; (use-package elpy
;;   :straight t
;;   :config
;;   (elpy-enable))

(use-package flx-ido
  :straight t
  :config
  (setq flx-ido-threshold 500)
  (flx-ido-mode 1))

(use-package ido
  :straight t
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

(use-package diminish
  :straight t
  :config
  (diminish 'auto-revert-mode))

(use-package ido-vertical-mode
  :straight t
  :config
  (ido-vertical-mode))

(use-package request
  :straight t
  :config
  (setq request-curl-options '("--netrc")))

(use-package frame-fns
  :straight t)

(use-package frame-cmds
  :straight t
  :bind (("C-x 5 r" . rename-frame)))

(use-package icicles
  :straight  (icicles :type git :host github :repo "aarnej/icicles")
  :bind (("C-x 5 o" . icicle-select-frame)))

(use-package magit
  :straight (magit :type git :host github :repo "aarnej/magit")
  :commands (magit-status)
  :bind (("C-x v l" . magit-log-buffer-file)
         ("C-x v =" . magit-diff-buffer-file)
         ("C-x g" . magit-status))
  :config
  ;; (magit-define-popup-switch 'magit-fetch-popup
  ;;     ?t "Fetch all tags" "--tags")

  (setq magit-diff-refine-hunk 'all)
  (setq magit-fetch-arguments '("--tags"))
  (setq magit-gerrit-push-review-to-topic nil)
  (setq magit-log-arguments '("-n256" "--graph" "--decorate"))
  (setq magit-log-margin-spec '(28 1 magit-duration-spec))
  (setq magit-log-section-arguments '("-n256"))
  (setq magit-log-section-commit-count 20)
  (setq magit-log-show-margin nil)
  (setq magit-reflog-show-margin nil)
  (setq magit-refs-sections-hook '(magit-insert-error-header
                                   magit-insert-branch-description
                                   magit-insert-local-branches
                                   magit-insert-remote-branches))

  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" "--tags"))

  ;;(defface magit-aarne-review-name
  ;;  '((((class color) (background light)) :foreground "cyan")
  ;;    (((class color) (background  dark)) :foreground "cyan"))
  ;;  "Face for review name.")
  ;;
  ;;(defface magit-aarne-review-status
  ;;  '((((class color) (background light)) :foreground "brightmagenta")
  ;;    (((class color) (background  dark)) :foreground "brightmagenta"))
  ;;  "Face for review status.")
  ;;
  ;;(defface magit-aarne-review-commit-status
  ;;  '((((class color) (background light)) :foreground "yellow")
  ;;    (((class color) (background  dark)) :foreground "yellow"))
  ;;  "Face for review status.")
  ;;
  ;;(defun* magit-insert-aarne-gerrit-reviews ()
  ;;  (let* ((logs (magit-git-lines "log" "@{upstream}.."))
  ;;         (commits (--filter (string-match "Change-Id" it) logs))
  ;;         (change-ids (or
  ;;                      (--map (substring it 15) commits)
  ;;                      (return-from magit-insert-aarne-gerrit-reviews)))
  ;;         (query (string-join
  ;;                 (--map (concat "change:" it) change-ids) " OR "))
  ;;         (req (request
  ;;               "https://gerrit.ericsson.se/a/changes/"
  ;;               :params (list (cons "q" query) '("o" . "ALL_REVISIONS"))
  ;;               :parser (lambda ()
  ;;                         (forward-line) ;; skip XSSI prevention magic
  ;;                         (json-read))
  ;;               :sync t))
  ;;         (data (request-response-data req))
  ;;         )
  ;;
  ;;    (magit-insert-section (reviews nil t)
  ;;      (magit-insert-heading "Reviews:")
  ;;      (mapc (lambda (it)
  ;;              (magit-insert-section (review)
  ;;                (insert
  ;;                 (propertize
  ;;                  (format "%s" (cdr (assoc '_number it))) 'face 'magit-aarne-review-name)
  ;;                 " "
  ;;                 (propertize
  ;;                  (format "%s" (cdr (assoc 'status it))) 'face 'magit-aarne-review-status)
  ;;                 " "
  ;;                 (propertize
  ;;                  (format "%s" (substring
  ;;                                (cdr (assoc 'current_revision it)) 0 9))
  ;;                          'face 'magit-aarne-review-status)
  ;;                 " "
  ;;                 (cdr (assoc 'subject it)) ?\n)))
  ;;            data)
  ;;      (insert ?\n)
  ;;      )
  ;;    )
  ;;  )

  (magit-add-section-hook 'magit-status-sections-hook
			              ;;'magit-insert-aarne-gerrit-reviews
                          'magit-insert-unpulled-from-pushremote)
  (magit-add-section-hook 'magit-status-sections-hook
			              'magit-insert-ignored-files
                          nil t)
  (magit-add-section-hook 'magit-status-sections-hook
			              'magit-insert-worktrees
                          nil t)
  )

;; (use-package forge
;;   :straight t
;;   ;; :after magit
;;   )

;; (use-package magit-gerrit
;;   :straight t
;;   :config
;;   (setq-default magit-gerrit-ssh-creds ""))

(use-package projectile
  :straight t
  ;; :after (diminish)
  :bind ("C-x f" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-mode-line '(:eval (format " Pj[%s]" (projectile-project-name))))
  (diminish 'projectile-mode))

(use-package recentf
    :straight t)

;; (use-package robot-mode
;;   :straight t
;;   :mode "\\.robot\\'")

(use-package undo-tree
  :straight t
  ;; :after (diminish)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-mode-lighter " Undo-T")
  (diminish 'undo-tree-mode))

(use-package whitespace
  :straight t
  ;; :after (diminish)
  :config
  (setq whitespace-global-modes '(python-mode))
  (setq whitespace-line-column 79)
  (setq whitespace-style '(face trailing tab-mark))

  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook 'makefile-gmake-mode-hook #'whitespace-mode)
  (diminish 'global-whitespace-mode)
  )

(use-package which-key
  :straight t
  :config
  (setq which-key-idle-delay 2.0)
  (which-key-mode))

(use-package eslint-fix
  :straight t)

;; (use-package gh
;; :straight t)

(use-package vlf
  :straight t
  :config
  (require 'vlf-setup))

;; Global key mapping

(setq my-keys-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c <left>")  'windmove-left)
    (define-key map (kbd "C-c <right>") 'windmove-right)
    (define-key map (kbd "C-c <up>")    'windmove-up)
    (define-key map (kbd "C-c <down>")  'windmove-down)
    (define-key map (kbd "C-c b p") 'previous-buffer)
    (define-key map (kbd "C-c b n") 'next-buffer)
    (define-key map (kbd "C-x C-b") 'ibuffer)
    (define-key map (kbd "C-c s") 'yank-isearch-string)
    map))

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t)

(my-keys-minor-mode 1)

;; (keyboard-translate ?\C-h ?\C-?)  ; translate 'C-h' to DEL
;; (keyboard-translate ?\C-? ?\C-h)  ; translate DEL to 'C-h'.

(provide 'init)
