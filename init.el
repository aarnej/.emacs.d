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

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1)))))

;; (use-package frame-fns
;;   :load-path "lisp/emacswiki.org")
;;
;; (use-package frame-cmds
;;   :load-path "lisp/emacswiki.org")

;; (use-package doremi-frm
;;   :load-path "lisp/emacswiki.org"
;;   :config
;;   (defalias 'doremi-prefix (make-sparse-keymap))
;;   (defvar doremi-map (symbol-function 'doremi-prefix)
;;     "Keymap for Do Re Mi commands.")
;;   (define-key global-map "\C-xt" 'doremi-prefix)
;;   (define-key doremi-map "a" 'doremi-all-faces-fg+)    ; "All"
;;   (define-key doremi-map "c" 'doremi-bg+)              ; "Color"
;;   (define-key doremi-map "f" 'doremi-face-fg+)         ; Face"
;;   (define-key doremi-map "h" 'doremi-frame-height+)
;;   (define-key doremi-map "t" 'doremi-font+)            ; "Typeface"
;;   (define-key doremi-map "u" 'doremi-frame-configs+)   ; "Undo"
;;   (define-key doremi-map "x" 'doremi-frame-horizontally+)
;;   (define-key doremi-map "y" 'doremi-frame-vertically+)
;;   (define-key doremi-map "z" 'doremi-font-size+))      ; "Zoom"

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
  :load-path "lisp/magit/lisp"
  :bind (("C-x v l" . magit-log-buffer-file)
         ("C-x g" . magit-status)))

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

