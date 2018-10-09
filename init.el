;; GNU Emacs 26.1

(server-start)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'use-package)

(use-package adoc-mode
  :ensure t
  :mode (("\\.adoc\\'" . adoc-mode)))

(use-package ag
  :ensure t
  :bind ("\C-cg" .
	 (lambda () (interactive)
	   (setq current-prefix-arg '(4)) ; C-u
	   (call-interactively 'ag))))

(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings))

(use-package elpy
  :ensure t)

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

(use-package magit
  :ensure t
  :bind (("C-x v l" . magit-log-buffer-file)))

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
  :config
  (add-to-list 'auto-mode-alist '("\\.robot\\'" . robot-mode)))

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
