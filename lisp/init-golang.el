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

(require-package 'auto-complete)
(require-package 'go-autocomplete)

(require 'auto-complete-config)
(require 'go-autocomplete)
(ac-config-default)

(provide 'init-golang)
