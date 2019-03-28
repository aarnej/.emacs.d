(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 60)
 '(ag-arguments (quote ("--smart-case" "--stats" "--width=100")))
 '(ag-highlight-search t)
 '(auto-save-default nil)
 '(column-number-mode t)
 '(confirm-nonexistent-file-or-buffer t)
 '(css-indent-offset 2)
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-django elpy-module-sane-defaults)))
 '(elpy-rpc-backend "jedi")
 '(elpy-shell-echo-input-cont-prompt nil)
 '(fill-column 79)
 '(flycheck-shellcheck-follow-sources nil)
 '(git-commit-fill-column 70)
 '(global-whitespace-mode t)
 '(ibuffer-elide-long-columns nil)
 '(ibuffer-formats
   (quote
    ((mark modified read-only locked " "
           (name 60 60 :left :elide)
           " " filename-and-process " ")
     (mark " "
           (name 16 -1)
           " " filename))))
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(kill-ring-max 1000)
 '(magit-diff-refine-hunk (quote all))
 '(magit-fetch-arguments (quote ("--tags")))
 '(magit-gerrit-push-review-to-topic nil)
 '(magit-log-arguments (quote ("-n256" "--graph" "--decorate")))
 '(magit-log-margin-spec (quote (28 1 magit-duration-spec)))
 '(magit-log-section-arguments (quote ("-n256")))
 '(magit-log-section-commit-count 20)
 '(magit-log-show-margin nil)
 '(magit-reflog-show-margin nil)
 '(magit-refs-sections-hook
   (quote
    (magit-insert-error-header magit-insert-branch-description magit-insert-local-branches magit-insert-remote-branches)))
 '(make-backup-files nil)
 '(mode-line-format
   (quote
    ("" mode-line-client mode-line-modified mode-line-frame-identification
     (:eval
      (format "[%s]"
              (projectile-project-name)))
     " " mode-line-buffer-identification mode-line-misc-info mode-line-position mode-line-remote mode-line-end-spaces)))
 '(mode-require-final-newline nil)
 '(package-selected-packages
   (quote
    (transient request rjsx-mode flycheck dired-filter python-docstring json-mode yaml-mode xterm-color xcscope use-package undo-tree projectile popup js2-mode ido-vertical-mode helm-core flx-ido fiplr browse-kill-ring ag adoc-mode)))
 '(projectile-mode-line
   (quote
    (:eval
     (format " Pj[%s]"
             (projectile-project-name)))))
 '(python-docstring-sentence-end-double-space nil)
 '(python-fill-docstring-style (quote django))
 '(python-shell-interpreter "python3")
 '(recentf-max-saved-items 1000)
 '(recentf-mode t)
 '(request-curl-options (quote ("--netrc")))
 '(require-final-newline nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(tab-width 4)
 '(tar-mode-show-date t)
 '(truncate-partial-width-windows nil)
 '(undo-tree-mode-lighter " Undo-T")
 '(vc-display-status nil)
 '(which-function-mode t)
 '(whitespace-global-modes (quote (python-mode)))
 '(whitespace-line-column 79)
 '(whitespace-style (quote (face trailing lines-tail tab-mark)))
 '(yaml-block-literal-electric-alist (quote ((124 . "") (62 . "")))))
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
