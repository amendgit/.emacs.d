;;; idle-require-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "idle-require" "idle-require.el" (21916 53232
;;;;;;  0 0))
;;; Generated autoloads from idle-require.el

(autoload 'idle-require "idle-require" "\
Add FEATURE to `idle-require-symbols'.
FILENAME and NOERROR are provided for compatibility to `require'.  If FILENAME
is non-nil, it is added instead of FEATURE.  NOERROR has no effect as that is
the default.

\(fn FEATURE &optional FILENAME NOERROR)" nil nil)

(autoload 'idle-require-mode "idle-require" "\
Load unloaded autoload functions when Emacs becomes idle.
If `idle-require-symbols' is a list of files, those will be loaded.
Otherwise all autoload functions will be loaded.

Loading all autoload functions can easily triple Emacs' memory footprint.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; idle-require-autoloads.el ends here
