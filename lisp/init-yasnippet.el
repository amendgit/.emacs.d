(require-package 'yasnippet)

(yas-global-mode 1)

(setq yas-snippet-dirs
	'("~/.emacs.d/misc/snippets" 
	  "~/.emacs.d/misc/yasmate/snippets" ))

(provide 'init-yasnippet)