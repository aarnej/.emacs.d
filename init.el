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

(use-package ag
  :ensure t
  :bind ("\C-cg" .
	 (lambda () (interactive)
	   (setq current-prefix-arg '(4)) ; C-u
	   (call-interactively 'ag)))
  :config
  (add-hook 'ag-mode-hook (lambda () (setq truncate-lines t))))

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
  (ido-mode 1))

(use-package ido-vertical-mode
  :ensure t
  :config
  (ido-vertical-mode))

(use-package request
  :ensure t
  )

(use-package magit
  :load-path "lisp/magit/lisp"
  :commands (magit-status)
  :bind (("C-x v l" . magit-log-buffer-file)
         ("C-x v =" . magit-diff-buffer-file)
         ("C-x g" . magit-status))
  :config
  (magit-define-popup-switch 'magit-fetch-popup
      ?t "Fetch all tags" "--tags")

  (defun magit-insert-aarne-gerrit-reviews ()
    (let* ((commits (--filter (string-match "Change-Id" it)
                              (magit-git-lines "log" "@{upstream}..")))
           (change-ids (--map (substring it 15) commits))
           (query (string-join
                   (--map (concat "change:" it) change-ids) " OR "))
           (json ["1" "2" "3"])
           (req (request
                 "https://gerrit.ericsson.se/a/changes/"
                 :params (list (cons "q" query))
                 :parser (lambda ()
                           (forward-line) ;; skip XSSI prevention magic
                           (json-read))
                 :sync t))
           (data (request-response-data req))
           )

      (magit-insert-section (reviews nil t)
        (magit-insert-heading "Reviews")
        (mapc (lambda (it)
                (magit-insert-section (review)
                  (insert (cdr (assoc 'subject it)) ?\n)))
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
  (add-hook 'before-save-hook 'whitespace-cleanup))

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

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (delete 'indentation::space (symbol-value 'whitespace-style))))

(load-file "~/.emacs.d/defun.el")
(global-set-key (kbd "C-c s")  'yank-isearch-string)
(add-hook 'find-file-hook 'commit_msg_hook)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'magit-clean 'disabled nil)
