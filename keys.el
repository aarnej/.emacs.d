(define-key input-decode-map "\e[A" [C-up])
(define-key input-decode-map "\e[B" [C-down])
(define-key input-decode-map "\e[C" [C-right])
(define-key input-decode-map "\e[D" [C-left])

(setq aj-mode-map (make-sparse-keymap))
(define-key aj-mode-map         (kbd "M-<up>")      'move-text-up)
(define-key aj-mode-map         (kbd "M-<down>")    'move-text-down)
(define-key aj-mode-map         (kbd "ESC <up>")    'move-text-up)
(define-key aj-mode-map         (kbd "ESC <down>")  'move-text-down)
;;                                   "C-c C-  reserved by web-mode
(define-key aj-mode-map         (kbd "C-c SPC")     'ace-jump-mode)
;; (define-key aj-mode-map         (kbd "C-c .")       'popper-toggle-latest)
(define-key aj-mode-map         (kbd "C-c <down>")  'windmove-down)
(define-key aj-mode-map         (kbd "C-c <left>")  'windmove-left)
(define-key aj-mode-map         (kbd "C-c <right>") 'windmove-right)
(define-key aj-mode-map         (kbd "C-c <up>")    'windmove-up)
(define-key aj-mode-map         (kbd "C-c x")       'er/expand-region)
(define-key aj-mode-map         (kbd "C-c i")       'imenu)
;; (define-key lsp-mode-map        (kbd "C-c l")       lsp-command-map)
(define-key aj-mode-map         (kbd "C-c ma")      'mc/vertical-align-with-space)
(define-key aj-mode-map         (kbd "C-c md")      'me/duplicate-backward)
(define-key aj-mode-map         (kbd "C-c me")      'mc/edit-lines)
(define-key aj-mode-map         (kbd "C-c mx")      'mc/mark-more-like-this-extended)
(define-key projectile-mode-map (kbd "C-c p")       projectile-command-map)
(define-key aj-mode-map         (kbd "C-c r")       #'rg-menu)
(define-key aj-mode-map         (kbd "C-c s")       'yank-isearch-string)
(define-key aj-mode-map         (kbd "C-c u")       'undo)
(define-key aj-mode-map         (kbd "C-x C-b")     'ibuffer)
(define-key projectile-mode-map (kbd "C-x f")       'projectile-find-file)
(define-key aj-mode-map         (kbd "C-x g")       'magit-status)
(define-key aj-mode-map         (kbd "C-x v l")     'magit-log-buffer-file)
(define-key aj-mode-map         (kbd "C-x v =")     'magit-diff-buffer-file)
(define-key aj-mode-map         (kbd "M-z")         'zap-up-to-char)

(global-unset-key (kbd "C-x m")) ;; send mail
(global-unset-key (kbd "C-z")) ;; suspend frame

(define-minor-mode aj-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t)

(aj-mode 1)





