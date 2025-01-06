;;;; This is the sniem config

;;; Sniem
(gpack sniem
  :repo "SpringHan/sniem"
  :hook (after-init-hook . global-sniem-mode))

(spring/extra-add-to-list "~/.emacs.d/third-party/sniem/sniem")
;; (spring/native-compile-or-load "~/.emacs.d/third-party/sniem/sniem" nil t)

;;; Vars
(setq sniem-center-message nil
      sniem-mark-message nil
      sniem-delete-message nil
      sniem-change-message nil
      sniem-yank-message nil
      sniem-pair-message nil
      sniem-expand-region-message nil
      sniem-paste-message ""
      sniem-macro-message nil
      sniem-insert-mode-cursor '(bar . 3)
      sniem-object-catch-auto-backward t)
(add-to-list 'sniem-close-mode-alist 'eaf-mode)
(add-to-list 'sniem-close-mode-alist 'emulting-mode)
(add-to-list 'sniem-close-mode-alist 'telega-root-mode)
(add-to-list 'sniem-close-mode-alist 'telega-chat-mode)
(add-to-list 'sniem-normal-mode-alist 'helpful-mode)
(add-to-list 'sniem-close-mode-alist 'lsp-bridge-call-hierarchy-mode)
(add-to-list 'sniem-close-mode-alist 'copilot-chat-mode)
(add-to-list 'sniem-close-mode-alist 'copilot-chat-prompt-mode)

;;; Hook
(defvar spring/sniem-auto-save-timer nil
  "The timer for auto-save.")

(defvar spring/sniem-auto-save-blacklist '("COMMIT_EDITMSG")
  "The blacklist of buffers to auto save.")

(add-hook 'sniem-insert-to-normal-hook
          (lambda ()
            (unless (or (timerp spring/sniem-auto-save-timer)
                        defining-kbd-macro
                        executing-kbd-macro)
              (setq-local spring/sniem-auto-save-timer
                          (run-with-timer
                           2 nil
                           (lambda (current-buf)
                             (when (get-buffer current-buf)
                               (with-current-buffer current-buf
                                 (when (and (buffer-file-name (get-buffer current-buf))
                                            (not (sniem--mems current-buf
                                                              spring/sniem-auto-save-blacklist))
                                            (buffer-modified-p)
                                            sniem-normal-mode)
                                   (save-buffer))))
                             (setq spring/sniem-auto-save-timer nil))
                           (buffer-name))))))
(add-hook 'sniem-normal-to-insert-hook
          (lambda ()
            (when (timerp spring/sniem-auto-save-timer)
              (cancel-timer spring/sniem-auto-save-timer)
              (setq-local spring/sniem-auto-save-timer nil))))

;;; Keymap settings
(sniem-leader-set-key
 "q" 'sniem-keypad
 "h" 'sniem-keypad
 "'" 'sniem-keypad
 ;; File
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
 "w SPC" 'spring/move-to-window
 ;; Configs
 "zi" 'open-config-file
 "zmw" 'window-move
 "fe" 'eshell
 "fi" 'indent-region
 "ze" 'spring/edit-snippets
 "zE" 'spring/insert-license
 "zo" 'open-the-dir
 "zO" 'spring/open-scratch
 "zc" (lambda () (interactive) (emulting 'config))

 ;; Plugins Keymap
 ;; EAF-git
 "g" 'eaf-open-git
 ;; Quickrun
 "r" 'spring-run-code
 ;; evil-nerd-commenter
 "Ci" 'evilnc-comment-or-uncomment-lines
 "Cl" 'evilnc-quick-comment-or-uncomment-to-the-line
 "Cc" 'evilnc-copy-and-comment-lines
 ;; Dired-mode
 "D" 'dired-jump
 ;; Treemacs
 "tt" 'treemacs
 "ts" 'treemacs-select-window
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
 ;; "nt" 'netease-cloud-music
 ;; "nT" 'eaf-open-netease-cloud-music
 ;; "nr" 'netease-cloud-music-change-repeat-mode
 ;; "na" 'netease-cloud-music-add-header-lyrics
 ;; "nd" 'netease-cloud-music-delete-header-lyrics
 ;; "n<" 'netease-cloud-music-seek-backward
 ;; "n>" 'netease-cloud-music-seek-forward
 ;; "n SPC" 'netease-cloud-music-pause-or-continue
 ;; "nn" 'netease-cloud-music-play-next-song
 ;; "np" 'netease-cloud-music-play-previous-song
 ;; "nN" 'netease-cloud-music-random-play
 ;; "n/" 'netease-cloud-music-ask-play

 ;; IAlign
 "i" 'ialign

 ;; Copilot
 "Cd" 'spring/copilot-chat-display
 "Ce" 'copilot-chat-explain
 "Cf" 'copilot-chat-fix
 "Cr" 'copilot-chat-reset

 ;; Other functions
 "dc" 'spring/copy-directory-path
 "Mf" 'mark-defun
 "Mh" 'mark-whole-buffer
 "fr" 'recentf-open-files
 "fT" 'spring/terlat-translate-insert
 "fc" 'spring/test-color
 "fC" 'spring/input-char-number
 "fS" 'spring/set-value-at-point
 "fp" 'spring/char-to-string-output
 "fE" 'spring/eval-expression
 ", SPC" 'sniem-digit-argument-or-fn
 ",t" 'spring/compile-test
 ",g" 'magit-status
 "RET" 'sniem-object-catch
 "e" 'er/expand-region
 "B" 'spring/vue-build
 "~" (lambda () (interactive) (sniem-change-mode 'normal))
 "np" 'spring/notepad
 "$" 'spring/set-st-working-dir)
(sniem-set-keyboard-layout 'colemak)
(sniem-normal-set-key
 "/" 'swiper
 "?" 'swiper
 ;; move-text
 "M-e" 'move-text-down
 "M-u" 'move-text-up

 ;; Nice key
 "=" 'spring/kill-space-line-content
 "~" (lambda () (interactive) (sniem-change-mode 'motion))
 ;; "(" (lambda () (interactive) (desktop-init-reopen-files t))
 ;; ")" 'desktop-init-kill-all-files
 "-" 'copilot-chat-list
 "$" 'spring/set-st-working-dir)
(sniem-set-quit-insert-key "<C-tab>")
(global-set-key (kbd "C--") sniem-leader-keymap)
;;; Third-party
;; (setq netease-cloud-music-jump-function 'sniem-digit-argument-get)

(provide 'init-sniem)
