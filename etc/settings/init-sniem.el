;;;; This is the sniem config

;;; Sniem
(gpack sniem
  :repo "SpringHan/sniem"
  :hook (after-init-hook . global-sniem-mode))

;;; Vars
(setq sniem-center-message nil
      sniem-mark-message nil
      sniem-delete-message nil
      sniem-change-message nil
      sniem-yank-message nil
      sniem-macro-message nil)
(add-to-list 'sniem-close-mode-alist 'eaf-mode)

(sniem-leader-set-key
 "u" 'previous-line
 "e" 'next-line
 "q" 'sniem-keypad
 "h" 'sniem-keypad
 ;; File
 "ff" 'find-file
 "fx" 'kill-current-buffer
 ;; Window
 "wx" 'delete-window
 "wo" 'delete-other-windows
 "wh" 'split-window-below
 "wv" 'split-window-right
 "wc" 'ace-window
 "wr" 'windresize
 "wk" 'kill-buffer-and-window
 "wm" 'window-move
 ;; Configs
 "zi" 'open-config-file
 "zmw" 'window-move
 "fe" 'eshell
 "fi" 'indent-region
 "ze" 'spring/edit-snippets
 "zo" 'open-the-dir
 "zO" 'spring/open-scratch
 "zc" 'open-etc-config

 ;; Plugins Keymap
 ;; Magit
 "g" 'magit-status
 ;; Quickrun
 "r" 'spring-run-code
 ;; evil-nerd-commenter
 "ci" 'evilnc-comment-or-uncomment-lines
 "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
 "cc" 'evilnc-copy-and-comment-lines
 "cp" 'evilnc-comment-or-uncomment-paragraphs
 ;; Dired-mode
 "R" 'ranger
 ;; Treemacs
 "tt" 'treemacs
 "ts" 'treemacs-select-window
 ;; Avy
 "ac" 'avy-goto-char

 ;; Other functions
 "m" 'nil
 "mf" 'mark-defun
 "mh" 'mark-whole-buffer
 "fr" 'recentf-open-files
 "." 'xref-find-definitions
 "ft" 'spring/terlat-translate
 "fT" 'spring/terlat-translate-insert
 "fc" 'spring/test-color
 "fC" 'spring/input-char-number
 "fS" 'spring/set-variable-region
 "fv" 'spring/print-vars-value
 ",h" 'spring/hugo)
(sniem-set-keyboard-layout 'colemak)
(sniem-normal-set-key "/" 'swiper)

(provide 'init-sniem)
