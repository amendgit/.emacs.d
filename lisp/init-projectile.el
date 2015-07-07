(require-package 'projectile)
(require-package 'helm-projectile)
(require 'projectile)
(require 'helm)
(require 'helm-projectile)

(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-find-file 'helm-projectile-find-file)
(helm-projectile-on)

(provide 'init-projectile)
