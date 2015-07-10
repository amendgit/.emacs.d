(require-package 'neotree)

(require 'neotree)

;; (setq projectile-switch-project-action 'neotree-projectile-action)

(when neo-persist-show
  (add-hook 'popwin:before-popup-hook
            (lambda () (setq neo-persist-show nil)))
  (add-hook 'popwin:after-popup-hook
            (lambda () (setq neo-persist-show t))))

(defun neotree-projectile-toggle ()
  "Toggle neotree using the projectile root"
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((project-dir (projectile-project-root))
         (file-name  (buffer-file-name)))
      (if project-dir
          (progn
            (neotree-dir project-dir)
            (neotree-find file-name))
        (message "Could not find project root")))))

(provide 'init-neotree)
