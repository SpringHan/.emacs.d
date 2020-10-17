;;;; The evil config for my emacs.

;;; Evil
(package-require
 'evil-leader
 :hook '(after-init-hook . global-evil-leader-mode))
(package-require
 'evil
 :hook '(global-evil-leader-mode-hook . (lambda () (evil-mode t)))
 :hook '(Info-mode-hook . (lambda () (evil-emacs-state)))
 :hook '(xref--xref-buffer-mode-hook . (lambda () (evil-emacs-state))))

;;; Evil-nerd-commenter
(package-require
 'evil-nerd-commenter)

;;; The functions to set the evil-keys
(defun set-movement-evil-states-keys (key def)
	(dolist (state '(normal visual motion))
		(evil-global-set-key state key def)))

(defun set-in-navigation-evil-states (key def)
	(dolist (state '(normal insert visual replace))
		(evil-global-set-key state key def)))

;;; Defines

;;; Cursors' movement
(set-movement-evil-states-keys "u" 'evil-previous-line)
(set-movement-evil-states-keys "e" 'evil-next-line)
(set-movement-evil-states-keys "n" 'evil-backward-char)
(set-movement-evil-states-keys "i" 'evil-forward-char)
(set-movement-evil-states-keys "H" 'evil-insert-line)
(set-movement-evil-states-keys "s" 'eval-last-sexp)

(set-movement-evil-states-keys "I" '(lambda () (interactive) (evil-forward-char 5)))
(set-movement-evil-states-keys "N" '(lambda () (interactive) (evil-backward-char 5)))
(set-movement-evil-states-keys "E" '(lambda () (interactive) (evil-next-line 5)))
(set-movement-evil-states-keys "U" '(lambda () (interactive) (evil-previous-line 5)))

;;; Other movement
(evil-global-set-key 'normal "S" 'save-buffer)
(evil-global-set-key 'normal "Q" 'save-buffers-kill-terminal)
(evil-global-set-key 'normal "l" 'undo)
(evil-global-set-key 'normal "L" 'undo-tree-redo)
(evil-global-set-key 'normal "h" 'evil-insert)
(evil-global-set-key 'motion ";" 'counsel-M-x)

(evil-global-set-key 'normal "/" 'swiper)
(evil-global-set-key 'insert (kbd "M-p") 'previous-line)
(evil-global-set-key 'insert (kbd "M-n") 'next-line)
(evil-global-set-key 'insert (kbd "C-p") 'evil-normal-state)
(evil-global-set-key 'insert (kbd "C-o") 'evil-open-below)
(evil-global-set-key 'insert (kbd "C-S-o") 'evil-open-above)
(evil-global-set-key 'replace (kbd "C-p") 'evil-normal-state)

;;; Leader
(setq evil-leader/leader ",")
(evil-leader/set-key
	;; File
	"ff" 'find-file
	;; Window
	"wx" 'delete-window
	"wo" 'delete-other-windows
	"wh" 'split-window-below
	"wv" 'split-window-right
	"wc" 'ace-window
	"wr" 'windresize
	"wk" 'kill-buffer-and-window
	;; ;; Tab
	;; "tS" 'tab-bar-select-tab-by-name
	;; "to" 'tab-bar-close-other-tabs
	;; "tn" 'tab-bar-new-with-buffer
	;; "tN" 'tab-bar-new
	;; "ts" 'spring/tab-bar-new-scratch
	;; "tn" 'tab-bar-switch-to-next-tab
	;; "tp" 'tab-bar-switch-to-prev-tab
	;; "tx" 'tab-bar-close-tab
	;; Configs
	"zi" 'open-config-file
	"zmw" 'window-move
	"fe" 'eshell
	"ze" 'spring/edit-snippets
	"zo" 'open-the-dir
	"," 'spring/movement-up
	"." 'spring/movement-down

	;; Plugins Keymap
	;; Magit
	"'m" 'magit-status
	;; org
	"og" 'org-agenda
	"oc" 'org-capture
	"os" 'org-timer-start
	"oS" 'org-timer-set-timer
	"oe" 'org-timer-stop
	"op" 'org-timer-pause-or-continue
	;; netease-cloud-music
	"mt" 'netease-cloud-music
	"mr" 'netease-cloud-music-change-repeat-mode
	;; Caps_Lock
	"g" 'caps-lock-mode
	;; Github Explore
	"G" 'github-explorer
	;; Quickrun
	"r" 'quickrun-shell
	;; command-log
	"lk" 'spring/open-or-close-command-log-mode
	"lK" 'clm/command-log-clear
	;; vc-msg
	"s" 'vc-msg-show
	;; evil-nerd-commenter
	"ci" 'evilnc-comment-or-uncomment-lines
	"cl" 'evilnc-quick-comment-or-uncomment-to-the-line
	"cc" 'evilnc-copy-and-comment-lines
	"cp" 'evilnc-comment-or-uncomment-paragraphs
	;; Dired-mode
	"D" 'dired
	;; diff-hl
	"dn" 'diff-hl-next-hunk
	"dp" 'diff-hl-previous-hunk
	;; counsel-etags
	"el" 'counsel-etags-list-tag
	;; outline
	"fs" 'outline-show-entry
	"fh" 'outline-hide-entry
	"fa" 'outline-show-all
	"fA" 'outline-hide-body
	;; Treemacs
	"tt" 'treemacs
	"ts" 'treemacs-select-window
	;; Other functions
	"mf" 'mark-defun
	"fr" 'recentf-open-files)

(provide 'init-evil)
