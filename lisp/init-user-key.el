;;----------------------------------------------------------------------------
;; User define shortcut keys.
;;----------------------------------------------------------------------------
(define-prefix-command 'user-key-map)
(global-set-key (kbd "C-z") 'user-key-map)
(global-set-key (kbd "C-z F") 'helm-dired-action)
(global-set-key (kbd "C-z s") 'helm-projectile-grep)
(global-set-key (kbd "C-z y") 'helm-c-yas-complete)

(global-set-key (kbd "M-<up>") 'scroll-down-line)
(global-set-key (kbd "M-<down>") 'scroll-up-line)

;; M-s is free key.
(global-set-key (kbd "M-s") 'save-buffer)

;; Undo binding to key 'C-x u' 'C-_' and 'C-/', I do not need so many key for
;; undo.
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; Remap kill buffer to kill this buffer, cause kill this buffer more frequent.
(global-set-key "\C-xk" 'kill-this-buffer)

;; 'M-J' didn't bound to any command.
(global-set-key (kbd "M-J") 'neotree-project-dir)

;; Both 'C-M-j' and 'M-j' bind to indent-new-comment-line, but one is fine. Remap
;; M-j to helm-imenu.
(global-set-key (kbd "M-j") 'helm-imenu)

;; zap up to char.
(global-set-key (kbd "C-z z") 'zap-up-to-char)

;; These are free keys, use them.
(global-set-key (kbd "M-Z") 'helm-mini)
(global-set-key (kbd "M-T") 'neotree-toggle)
(global-set-key (kbd "M-N") 'new-frame)
(global-set-key (kbd "M-F") 'helm-projectile-find-file)
(global-set-key (kbd "M-P") 'helm-projectile-switch-project)
(global-set-key (kbd "M-E") 'eshell)

(provide 'init-user-key)
