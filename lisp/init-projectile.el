(require-package 'projectile)
(require-package 'sr-speedbar)
(require-package 'projectile-speedbar)

(require 'projectile)
(require 'sr-speedbar)
(require 'projectile-speedbar)

(setq speedbar-use-images nil)

; (with-current-buffer sr-speedbar-buffer-name
;   (setq window-size-fixed 'width))

(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-file-exists-remote-cache-expire nil)
(setq projectile-file-exists-local-cache-expire (* 5 60))

(global-set-key (kbd "C-x b") 'projectile-speedbar-open-current-buffer-in-tree)

(provide 'init-projectile)