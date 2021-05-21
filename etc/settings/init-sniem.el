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
      sniem-macro-message nil
      sniem-insert-mode-cursor '(bar . 3)
      sniem-expand-region-message nil)
(add-to-list 'sniem-close-mode-alist 'eaf-mode)
(add-to-list 'sniem-normal-mode-alist 'helpful-mode)

;;; Keymap settings
(sniem-leader-set-key
 "q" 'sniem-keypad
 "h" 'sniem-keypad
 "'" 'sniem-keypad
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
 "Ci" 'evilnc-comment-or-uncomment-lines
 "Cl" 'evilnc-quick-comment-or-uncomment-to-the-line
 "Cc" 'evilnc-copy-and-comment-lines
 "Cp" 'evilnc-comment-or-uncomment-paragraphs
 ;; Dired-mode
 "D" 'dired-jump
 ;; Treemacs
 "tt" 'treemacs
 "ts" 'treemacs-select-window
 ;; Avy
 "ac" 'avy-goto-char
 ;; Diff
 "d" 'nil
 "dp" 'diff-hl-previous-hunk
 "dn" 'diff-hl-next-hunk

 ;; Org mode
 "oc" 'org-capture
 "oa" 'org-agenda
 "os" 'org-timer-start
 "oS" 'org-timer-set-timer
 "oe" 'org-timer-stop
 "o SPC" 'org-timer-pause-or-continue

 ;; Netease-Cloud-Music
 "nt" 'netease-cloud-music
 "nr" 'netease-cloud-music-change-repeat-mode
 "na" 'netease-cloud-music-add-header-lyrics
 "nd" 'netease-cloud-music-delete-header-lyrics
 "n<" 'netease-cloud-music-seek-backward
 "n>" 'netease-cloud-music-seek-forward
 "n SPC" 'netease-cloud-music-pause-or-continue
 "nn" 'netease-cloud-music-play-next-song
 "np" 'netease-cloud-music-play-previous-song
 "nN" 'netease-cloud-music-random-play
 "n/" 'netease-cloud-music-ask-play

 ;; Nox
 "Nk" 'nox-shutdown

 ;; move-text
 "M-e" 'move-text-down
 "M-u" 'move-text-up

 ;; Other functions
 "dc" 'spring/copy-directory-path
 "Mf" 'mark-defun
 "Mh" 'mark-whole-buffer
 "fr" 'recentf-open-files
 "." 'spring/find-function
 "ft" 'spring/terlat-translate
 "fT" 'spring/terlat-translate-insert
 "fc" 'spring/test-color
 "fC" 'spring/input-char-number
 "fS" 'spring/set-value-at-point
 "fv" 'spring/print-vars-value
 "fE" 'eval-expression
 ",h" 'spring/hugo
 ", SPC" 'sniem-digit-argument-or-fn
 "RET" 'sniem-object-catch
 "e" 'er/expand-region)
(sniem-set-keyboard-layout 'colemak)
(sniem-normal-set-key "/" 'swiper)
(sniem-set-quit-insert-key "<C-tab>")

(provide 'init-sniem)
