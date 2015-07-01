(require-package 'go-mode)
(require 'go-mode-autoloads)
(add-hook 'before-save-hook 'gofmt-before-save)

(require-package 'auto-complete)
(require-package 'go-autocomplete)

(require 'auto-complete-config)
(require 'go-autocomplete)
(ac-config-default)

(provide 'init-golang)
