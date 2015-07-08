;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "23.3"))
  (when (version<= emacs-version "23.1")
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version<= emacs-version "24")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Less GC, more memory
;;----------------------------------------------------------------------------
;; By default Emacs will initiate GC every 0.76 MB allocated
;; (gc-cons-threshold == 800000).
;; we increase this to 512MB
;; @see http://www.gnu.org/software/emacs/manual/html_node/elisp/Garbage-Collection.html
(setq-default gc-cons-threshold (* 1024 1024 512)
              gc-cons-percentage 0.5)

;;----------------------------------------------------------------------------
;; Do not litter my fs tree.
;;----------------------------------------------------------------------------
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)
(require-package 'idle-require)

(require 'init-frame-hooks)

(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-editing-utils)

(require 'init-auto-complete)
(require 'init-sessions)

(require 'init-helm)
(require 'init-projectile)
(require 'init-linum)

(require 'idle-require)
(require 'init-xterm)
(require 'init-hippie-expand)
(require 'init-golang)
(require 'init-org)
(require 'init-markdown)
(require 'init-windows)
(require 'init-recentf)
(require 'init-neotree)
(require 'init-mmm)
(require 'init-fonts)
(require 'init-vc)
(require 'init-darcs)
(require 'init-git)
(require 'init-github)
(require 'init-compile)
(require 'init-crontab)
(require 'init-textile)
(require 'init-python-mode)
;; (require 'init-lisp)
(require 'init-slime)
(require 'init-proxies)
(require 'init-dash)
(require 'init-ledger)
(require 'init-grep)
(require 'init-ibuffer)
(require 'init-flycheck)
(require 'init-uniquify)
(require 'init-backup)
(require 'init-extra)

(require 'init-common-lisp)

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-user-key)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
