(setq-default
 sql-postgres-login-params '((user :default "")
                             (database :default "")
                             (server :default "")
                             (port :default 5434))
 custom-file null-device
 Buffer-menu-name-width 60
 auto-save-default nil
 before-save-hook '(whitespace-cleanup)
 confirm-nonexistent-file-or-buffer t
 create-lockfiles nil
 css-indent-offset 2
 custom-safe-themes '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)
 fill-column 79
 git-commit-fill-column 70
 ibuffer-formats '((mark modified read-only locked " "
                         (name 60 60 :left :elide)
                         " " filename-and-process " ")
                   (mark " "
                         (name 16 -1)
                         " " filename))
 indent-tabs-mode nil
 inhibit-startup-screen t
 initial-scratch-message nil
 js-indent-level 2
 kill-ring-max 100
 make-backup-files nil
 ; mode-require-final-newline 'ask
 require-final-newline 'ask
 python-fill-docstring-style 'django
 recentf-max-saved-items 1000
 ;; show-paren-style 'mixed
 split-height-threshold nil
 split-width-threshold nil
 tab-width 8
 tar-mode-show-date t
 truncate-partial-width-windows nil
 vc-display-status nil
 enable-recursive-minibuffers t
 minibuffer-depth-indicate-mode t
 ;; display-buffer-alist '((".*" . (display-buffer-same-window (inhibit-same-window . nil))))
 winner-dont-bind-my-keys t
 help-window-select t
 sentence-end-double-space nil
 grep-save-buffers nil
 cursor-type 'box
 blink-cursor-blinks 0
 blink-cursor-interval 0.25
 blink-cursor-delay 0
 w32-use-visible-system-caret nil
 scroll-conservatively 10000
 comint-scroll-to-bottom-on-input t
 eshell-scroll-to-bottom-on-input t
 enable-local-variables :all
 vc-follow-symlinks t
 auto-insert-query nil
 confirm-kill-emacs 'yes-or-no-p)

(add-hook 'eshell-mode-hook (lambda () (
  (define-key eshell-hist-mode-map (kbd "<up>") nil)
  (define-key eshell-hist-mode-map (kbd "<down>") nil))))

(load-theme 'tango-dark)

(menu-bar-mode 0)
(column-number-mode 1)
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))
;; (global-display-line-numbers-mode 1)
(global-whitespace-mode 1)
(global-visual-line-mode 0)
(global-superword-mode 0)
(recentf-mode 1)
(show-paren-mode 1)
(winner-mode 1)
(tool-bar-mode 0)
(if (and (fboundp 'server-running-p)
     (not (server-running-p)))
    (server-start))
(electric-indent-mode 0)
(savehist-mode 1)

(auto-insert-mode 1)
(setq auto-insert-alist nil)
(define-auto-insert '(restclient-mode . "REST client skeleton")
  '(""
    "# -*- restclient -*-" \n
    \n
    "#" \n
    "GET ")
)

(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

(put 'magit-clean 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'scroll-left 'disabled nil)

(add-hook 'emacs-lisp-mode-hook
          (lambda () (delete 'indentation::space (symbol-value 'whitespace-style))))
(add-hook 'find-file-hook 'commit_msg_hook)
(add-hook 'dired-mode-hook
          (lambda () (define-key dired-mode-map "Q" 'dired-do-query-replace-regexp)))
(add-hook 'shell-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'json-mode-hook #'hs-minor-mode)

(add-hook 'org-mode-hook #'aj/org-mode-hook)

(set-face-attribute 'default nil :family "Source Code Pro" :height 100 :background "gray14")

(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") '(lambda () (interactive) (flymake-goto-next-error 1 '(error warning) t)))
  (define-key flymake-mode-map (kbd "M-p") '(lambda () (interactive) (flymake-goto-prev-error 1 '(error warning) t))))

;; Temp fix for getting copypaste to work on WSLg
(when (and (getenv "WAYLAND_DISPLAY") (not (equal (getenv "GDK_BACKEND") "x11")))
  (setq
   interprogram-cut-function
   (lambda (text)
     (start-process "wl-copy" nil "wl-copy" "--trim-newline" "--type" "text/plain;charset=utf-8"  text))))
