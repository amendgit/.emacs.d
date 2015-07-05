(global-set-key (kbd "M-x") 'helm-M-x)

(global-unset-key (kbd "M-_"))
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-."))
(global-unset-key (kbd "C-x C-b"))

(global-set-key (kbd "M--") 'undo-tree-redo)
(global-set-key (kbd "C--") 'undo-tree-undo)

(define-prefix-command 'user-key-map)
(global-set-key (kbd "C-z") 'user-key-map)
(global-set-key (kbd "C-z p") 'helm-projectile-switch-project)
(global-set-key (kbd "C-z F") 'helm-dired-action)
(global-set-key (kbd "C-z s") 'helm-projectile-grep)
(global-set-key (kbd "C-z /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-z f") 'helm-projectile-find-file)
(global-set-key (kbd "C-z z") 'helm-mini)
(global-set-key (kbd "C-z y") 'helm-c-yas-complete)
(global-set-key (kbd "C-z t") 'neotree-toggle)
(global-set-key (kbd "C-z T") 'neotree-project-dir)

(global-set-key (kbd "C-x C-o") 'ffap)
(global-set-key (kbd "C-x b") 'helm-mini)

(provide 'init-user-key)
