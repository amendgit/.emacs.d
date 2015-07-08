(require-package 'projectile)
(require-package 'helm-projectile)
(require 'projectile)
(require 'helm)
(require 'helm-projectile)

(defun projectile-hashify-files (files-list)
  "Make the list of project files FILES-LIST ido friendly."
  (let ((files-table (make-hash-table :test 'equal))
        (files-to-uniquify nil))
    (dolist (current-file files-list files-table)
      (let ((basename (file-relative-name current-file (projectile-get-project-root))))
        (if (gethash basename files-table)
            (progn
              (puthash (uniquify-file current-file) current-file files-table)
              (when basename (push basename files-to-uniquify)))
          (puthash basename current-file files-table))))
    ;; uniquify remaining files
    (dolist (current-file (remove-duplicates files-to-uniquify :test 'string=))
      (puthash (uniquify-file (gethash current-file files-table)) (gethash current-file files-table) files-table)
      (remhash current-file files-table))
    files-table))


(setq projectile-enable-caching t)
(projectile-global-mode 1)

(setq projectile-switch-project-action 'helm-projectile)
(setq projectile-find-file 'helm-projectile-find-file)
(global-set-key [remap switch-to-buffer] 'helm-projectile-switch-to-buffer)
(helm-projectile-on)

(provide 'init-projectile)
