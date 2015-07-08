(require-package 'tidy)
(add-hook 'html-mode-hook (lambda () (tidy-build-menu html-mode-map)))

(require-package 'tagedit)
(after-load 'sgml-mode
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.(jsp|tmpl)\\'")

;; Note: ERB is configured in init-ruby-mode

(provide 'init-html)
