(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Buffer-menu-name-width 60)
 '(ag-arguments (quote ("--smart-case" "--statss")))
 '(column-number-mode t)
 '(confirm-nonexistent-file-or-buffer t)
 '(elpy-rpc-backend "jedi")
 '(elpy-shell-echo-input-cont-prompt nil)
 '(fill-column 79)
 '(git-commit-fill-column 70)
 '(global-whitespace-mode t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(kill-ring-max 1000)
 '(magit-diff-refine-hunk (quote all))
 '(magit-gerrit-push-review-to-topic nil)
 '(magit-log-arguments (quote ("-n256" "--graph" "--decorate")))
 '(magit-log-margin-spec (quote (28 1 magit-duration-spec)))
 '(magit-log-section-arguments (quote ("-n256")))
 '(magit-log-show-margin nil)
 '(magit-reflog-show-margin nil)
 '(magit-refs-sections-hook
   (quote
    (magit-insert-error-header magit-insert-branch-description magit-insert-local-branches magit-insert-remote-branches)))
 '(magit-status-sections-hook
   (quote
    (magit-insert-status-headers magit-insert-merge-log magit-insert-rebase-sequence magit-insert-am-sequence magit-insert-sequencer-sequence magit-insert-bisect-output magit-insert-bisect-rest magit-insert-bisect-log magit-insert-untracked-files magit-insert-unstaged-changes magit-insert-staged-changes magit-insert-stashes magit-insert-unpushed-to-pushremote magit-insert-unpushed-to-upstream-or-recent magit-insert-unpulled-from-pushremote magit-insert-unpulled-from-upstream magit-insert-ignored-files)))
 '(make-backup-files nil)
 '(mode-line-format
   (quote
    ("" mode-line-client mode-line-modified mode-line-frame-identification mode-line-buffer-identification " " mode-line-misc-info mode-line-position
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-remote mode-line-end-spaces)))
 '(mode-require-final-newline nil)
 '(package-selected-packages
   (quote
    (xcscope use-package undo-tree projectile popup magit-svn js2-mode ido-vertical-mode helm-core flx-ido fiplr browse-kill-ring ag adoc-mode)))
 '(python-fill-docstring-style (quote django))
 '(python-shell-interpreter "python3")
 '(recentf-max-saved-items 1000)
 '(recentf-mode t)
 '(require-final-newline nil)
 '(show-paren-mode t)
 '(show-paren-style (quote mixed))
 '(tab-width 4)
 '(truncate-partial-width-windows nil)
 '(vc-display-status nil)
 '(which-function-mode t)
 '(whitespace-global-modes (quote (python-mode)))
 '(whitespace-line-column 79)
 '(whitespace-style (quote (face trailing lines-tail tab-mark))))
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
