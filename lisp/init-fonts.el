;;; Character sets

(defcustom sanityinc/force-default-font-for-symbols nil
  "When non-nil, force Emacs to use your default font for symbols."
  :type 'boolean)

(defun sanityinc/maybe-use-default-font-for-symbols ()
  "Force Emacs to render symbols using the default font, if so configured."
  (when sanityinc/force-default-font-for-symbols
    (set-fontset-font "fontset-default" 'symbol (face-attribute 'default :family))))

(add-hook 'after-init-hook 'sanityinc/maybe-use-default-font-for-symbols)


;;; Changing font sizes

(require-package 'default-text-scale)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)


;;----------------------------------------------------------------------------
;; Make Chinese and English the same size for org mode.
;;----------------------------------------------------------------------------

;; Font for mac
(if *linux*
    (progn
      (set-face-font 'default "Consolas 10")
      (if (and (fboundp 'daemonp) (daemonp))
          (add-hook 'after-make-frame-functions
                    (lambda (frame)
                      (with-selected-frame frame
                        (set-fontset-font "fontset-default"
                                          'chinese-gbk "KaiTi 11"))))
        (set-fontset-font "fontset-default" 'chinese-gbk "KaiTi 11")))
  (set-face-font 'default "Consolas 13")
  (if (and (fboundp 'daemonp) (daemonp))
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (set-fontset-font "fontset-default"
                                      'chinese-gbk "STHeiti 14"))))
    (set-fontset-font "fontset-default" 'chinese-gbk "STHeiti 14")))


(provide 'init-fonts)
