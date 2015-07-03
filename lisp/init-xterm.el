(require 'init-frame-hooks)

(defun fix-up-xterm-control-arrows ()
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
               function-key-map)))
    (define-key map "\e[1;5A" [C-up])
    (define-key map "\e[1;5B" [C-down])
    (define-key map "\e[1;5C" [C-right])
    (define-key map "\e[1;5D" [C-left])
    (define-key map "\e[5A"   [C-up])
    (define-key map "\e[5B"   [C-down])
    (define-key map "\e[5C"   [C-right])
    (define-key map "\e[5D"   [C-left])))

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 1)))

(defun sanityinc/console-frame-setup ()
  (when (< emacs-major-version 23)
    (fix-up-xterm-control-arrows))
  (xterm-mouse-mode 1) ; Mouse in a terminal (Use shift to paste with middle button)
  (when (fboundp 'mwheel-install)
    (mwheel-install)))

(add-hook 'after-make-console-frame-hooks 'sanityinc/console-frame-setup)

(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] '(lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] '(lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)

  (defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

  (defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
  (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
  (process-send-string proc text)
  (process-send-eof proc))))

  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx)
)

(provide 'init-xterm)
