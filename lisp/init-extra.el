(require 'init-misc)
(require 'init-dash)
(require 'init-ledger)
;; Extra packages which don't require any configuration

(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)

(when *is-a-mac*
  (require-package 'osx-location))
(require-package 'regex-tool)

(provide 'init-extra)
