;;----------------------------------------------------------------------------
;; Unset some not frequent used keys.
;;----------------------------------------------------------------------------

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

(global-unset-key (kbd "M-_"))
(global-unset-key (kbd "C-_"))
(global-unset-key (kbd "C-."))
(global-unset-key (kbd "C-x C-b"))
(global-unset-key  (kbd "C-/")) ;; undo tree undo [C--]

(global-unset-key (kbd "C-x f")) ;; set-fill-column
(global-unset-key (kbd "M-SPC")) ;; conflict on mac

;;----------------------------------------------------------------------------
;; User define shortcut keys.
;;----------------------------------------------------------------------------
(define-prefix-command 'user-key-map)
(global-set-key (kbd "C-z") 'user-key-map)
(global-set-key (kbd "C-z p") 'helm-projectile-switch-project)
(global-set-key (kbd "C-z F") 'helm-dired-action)
(global-set-key (kbd "C-z s") 'helm-projectile-grep)
(global-set-key (kbd "C-z f") 'helm-projectile-find-file)
(global-set-key (kbd "C-z z") 'helm-mini)
(global-set-key (kbd "C-z y") 'helm-c-yas-complete)
(global-set-key (kbd "C-z t") 'neotree-toggle)
(global-set-key (kbd "C-z T") 'neotree-project-dir)
(global-set-key (kbd "C-z g") 'goto-line)
(global-set-key (kbd "C-z n") 'new-frame)
(global-set-key (kbd "C-z e") 'eshell)
(global-set-key (kbd "C-z j") 'helm-imenu)

;;----------------------------------------------------------------------------
;; Direct keys.
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

(global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "C-,") 'previous-buffer)
;; (global-set-key (kbd "C-.") 'next-buffer)

;; C-w cut, M-w copy, C-y paste, C-k kill line.

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; C-b back, C-f forward, C-p previous line, C-n next line.

(global-set-key (kbd "M--") 'undo-tree-redo)
(global-set-key (kbd "C--") 'undo-tree-undo)

;;----------------------------------------------------------------------------
;; Redefine some default keys.
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-x b") 'helm-mini)

;;----------------------------------------------------------------------------
;; For editing convients.
;;----------------------------------------------------------------------------

;; Just one space.
(global-set-key (kbd "C-z SPC") 'cycle-spacing)

;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Vimmy alternatives to M-^ and C-u M-^
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

;; Vimmy alternatives to M-^ and C-u M-^
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "C-x C-.") 'pop-global-mark)

;; multiple-cursors {{
;;
;; Multiple cursors.
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; From active region to multiple cursors:
(global-set-key (kbd "C-c c r") 'set-rectangular-region-anchor)
(global-set-key (kbd "C-c c c") 'mc/edit-lines)
(global-set-key (kbd "C-c c e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c c a") 'mc/edit-beginnings-of-lines)
;;
;; }}

;; ace-jump-mode {{
;;
(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "C-:") 'ace-jump-word-mode)
;;
;; }}

;; Delete from cursor to line head.
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(global-set-key [M-S-up] 'md/move-lines-up)
(global-set-key [M-S-down] 'md/move-lines-down)

(global-set-key (kbd "C-c p") 'md/duplicate-down)
(global-set-key (kbd "C-c P") 'md/duplicate-up)

(global-set-key (kbd "M-[") 'shift-region-left)
(global-set-key (kbd "M-]") 'shift-region-right)

;; Reslove key conflit with paredit.
(global-set-key (kbd "M-<up>") 'scroll-up-line)
(global-set-key (kbd "M-<down>") 'scroll-down-line)
(global-set-key (kbd "M-s") 'save-buffer)

(global-set-key (kbd "C-o") 'sanityinc/open-line-with-reindent)

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up, See http://bit.ly/h7mdIL



(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x" "C-c" "C-x 4" "C-x 5" "C-c ;" "C-c ; f" "C-c ' f" "C-x n" "C-x C-r" "C-x r" "C-z"))
(guide-key-mode 1)
(diminish 'guide-key-mode)

(provide 'init-user-key)
