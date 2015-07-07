(require-package 'neotree)

(require 'neotree)

(setq projectile-switch-project-action 'neotree-projectile-action)

(when neo-persist-show
  (add-hook 'popwin:before-popup-hook
            (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook
            (lambda () (setq neo-persist-show t))))

(setq projectile-switch-project-action 'helm-projectile-switch-project)

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name)
          (projectile-mode))
      (message "Could not find git project root."))))

(provide 'init-neotree)
