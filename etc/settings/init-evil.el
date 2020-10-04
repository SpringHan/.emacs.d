;;;; The evil config for my emacs.

;;; The functions to set the evil-keys
(defun set-in-evil-states (key def maps)
	(while maps
		(define-key (pop maps) key def)))

(defun set-in-navigation-evil-states (key def)
	(set-in-evil-states key def (list evil-motion-state-map
																		evil-normal-state-map
																		evil-visual-state-map)))

;;; Defines

;;; Cursors' movement
(set-in-navigation-evil-states "u" 'evil-previous-line)
(set-in-navigation-evil-states "e" 'evil-next-line)
(set-in-navigation-evil-states "n" 'evil-backward-char)
(set-in-navigation-evil-states "i" 'evil-forward-char)

(set-in-navigation-evil-states "I" '(lambda () (interactive) (evil-forward-char 5)))
(set-in-navigation-evil-states "N" '(lambda () (interactive) (evil-backward-char 5)))
(set-in-navigation-evil-states "E" '(lambda () (interactive) (evil-next-line 5)))
(set-in-navigation-evil-states "U" '(lambda () (interactive) (evil-previous-line 5)))

;;; Other movement
(define-key evil-normal-state-map "S" 'save-buffer)
(define-key evil-normal-state-map "Q" 'save-buffers-kill-terminal)
(define-key evil-normal-state-map "l" 'undo)
(define-key evil-normal-state-map "L" 'undo-tree-redo)
(define-key evil-motion-state-map ";" 'counsel-M-x)
(define-key evil-normal-state-map "h" 'evil-insert)

(set-in-navigation-evil-states "/" 'swiper)
(set-in-navigation-evil-states "k" 'evil-search-next)
(set-in-navigation-evil-states "K" 'evil-search-previous)
(set-in-navigation-evil-states "$" 'end-of-line)
(define-key evil-insert-state-map (kbd "M-p") 'previous-line)
(define-key evil-insert-state-map (kbd "M-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'evil-normal-state)
(define-key evil-insert-state-map (kbd "C-o") 'evil-open-below)
(define-key evil-insert-state-map (kbd "C-O") 'evil-open-above)
(define-key evil-replace-state-map (kbd "C-p") 'evil-normal-state)

;;; Leader
(setq evil-leader/leader ",")
(evil-leader/set-key
	;; Window
	"wx" 'delete-window
	"wo" 'delete-other-windows
	"wh" 'split-window-below
	"wv" 'split-window-right
	"wc" 'ace-window
	"wr" 'windresize
	"wk" 'kill-buffer-and-window
	;; Buffer
	"bx" 'kill-buffer
	"bX" 'kill-current-buffer
	"bs" 'switch-to-buffer
	"bk" 'kill-unwanted-buffer
	"bK" 'spring/kill-all-else-buffers
	"bp" 'previous-buffer
	"bn" 'next-buffer
	"bb" 'buffer-menu
	"bo" 'spring/open-scratch
	;; Tab
	"tS" 'tab-bar-select-tab-by-name
	"to" 'tab-bar-close-other-tabs
	"tn" 'tab-bar-new-with-buffer
	"tN" 'tab-bar-new
	"ts" 'spring/tab-bar-new-scratch
	;; Configs
	"zc" 'open-etc-config
	"zi" 'open-config-file
	"zv" 'spring/set-volume
	"zV" 'spring/show-volume
	"zu" 'spring/up-5-volume
	"zd" 'spring/down-5-volume
	"zl" 'spring/show-packages-required
	"zms" 'spring/search
	"zmb" 'eaf-open-bookmark
	"zmw" 'window-move
	"fe" 'eshell
	"ze" 'spring/edit-snippets
	"zo" 'open-the-dir
	"zO" 'spring/scratch-erase-contents

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
	"el" 'counsel-etags-list-tag)

(provide 'init-evil)
