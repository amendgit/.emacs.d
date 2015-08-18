(require-package 'go-mode)

(require 'go-mode-autoloads)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda() (
  local-set-key (kbd "M-.") 'godef-jump))
)
(add-hook 'go-mode-hook (lambda() (
  local-set-key (kbd "C-c i") 'go-goto-imports))
)
(add-hook 'go-mode-hook (lambda() (
  local-set-key (kbd "C-c C-r") 'go-remove-unused-imports))
)

(after-load 'go-mode
	(setq indent-tabs-mode nil)
	(setq default-tab-width 4))

(require-package 'company-go)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)))

(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

(provide 'init-golang)
