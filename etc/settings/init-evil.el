;;;; The evil config for my emacs.

;;; Evil
(gpack evil-leader
  :hook (after-init-hook . global-evil-leader-mode))
(gpack evil
  :hook ((global-evil-leader-mode-hook . (lambda () (evil-mode t)))
         (Info-selection-hook . (lambda () (evil-change-state 'emacs)))
         (ranger-mode-load-hook . (lambda () (evil-change-state 'emacs)))
         (xref--xref-buffer-mode-hook . (lambda () (evil-change-state 'emacs)))
         (netease-cloud-music-mode-hook . (lambda () (evil-change-state 'emacs)))))

;;; Evil-nerd-commenter
(gpack evil-nerd-commenter)

;;; Evil-Wildfire
(gpack evil-wildfire
  :disable
  :load-path "~/.emacs.d/third-party/evil-wildfire")

;;; The functions to set the evil-keys
(defun set-movement-evil-states-keys (key def)
  (dolist (state '(normal visual motion))
    (evil-global-set-key state key def)))

(defun set-in-navigation-evil-states (key def)
  (dolist (state '(normal insert visual replace))
    (evil-global-set-key state key def)))

;;; Motions
(evil-define-motion spring/movement-up ()
  "Movement up."
  :type line
  (spring/movement-with-middle-keyboard 'up))

(defun spring/movement-with-middle-keyboard (move)
  "Movement with middle keyboard."
  (interactive)
  (let ((times (spring/arg-with-middle-keyboard "Move: ")))
    (funcall (pcase move
               ('up 'evil-previous-line)
               ('down 'evil-next-line))
             times)))

(evil-define-motion spring/movement-down ()
  "Movement down."
  :type line
  (spring/movement-with-middle-keyboard 'down))

(evil-define-motion spring/+-5-lines ()
  "Move up 5 lines."
  :type line
  (evil-previous-line 5))

(evil-define-motion spring/--5-lines ()
  "Move down 5 lines."
  :type line
  (evil-next-line 5))

;;; Defines

;;; Cursors' movement
(set-movement-evil-states-keys "u" 'evil-previous-line)
(set-movement-evil-states-keys "e" 'evil-next-line)
(set-movement-evil-states-keys "n" 'evil-backward-char)
(set-movement-evil-states-keys "i" 'evil-forward-char)
(set-movement-evil-states-keys "H" 'evil-insert-line)
(set-movement-evil-states-keys "s" 'eval-last-sexp)
(set-movement-evil-states-keys (kbd "SPC") 'spring/evil-digit-argument)
(set-movement-evil-states-keys (kbd "C-a") '(lambda () (interactive) (spring/number-add-delete-one t)))
(set-movement-evil-states-keys (kbd "C-d") '(lambda () (interactive) (spring/number-add-delete-one nil)))

(set-movement-evil-states-keys "I" '(lambda () (interactive) (evil-forward-char 5)))
(set-movement-evil-states-keys "N" '(lambda () (interactive) (evil-backward-char 5)))
(set-movement-evil-states-keys "E" 'spring/--5-lines)
(set-movement-evil-states-keys "U" 'spring/+-5-lines)

;;; Other movement
(evil-global-set-key 'visual "h" 'evil-insert)
(evil-global-set-key 'normal "S" 'save-buffer)
(evil-global-set-key 'normal "Q" 'save-buffers-kill-terminal)
(evil-global-set-key 'normal "l" 'undo)
(evil-global-set-key 'normal "h" 'evil-insert)
(evil-global-set-key 'motion ";" 'evil-ex)
(set-movement-evil-states-keys "." 'spring/movement-down)
(set-movement-evil-states-keys ">" 'spring/movement-up)
(set-movement-evil-states-keys "k" 'evil-search-next)
(set-movement-evil-states-keys "K" 'evil-search-previous)
(set-movement-evil-states-keys "?" 'evil-search-forward)

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
  "fi" 'indent-region
  "ze" 'spring/edit-snippets
  "zo" 'open-the-dir
  "zO" 'spring/open-scratch
  "zc" 'open-etc-config

  ;; Plugins Keymap
  ;; Magit
  "g" 'magit-status
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
  ;; LightGit
  ;; "g" 'lightgit
  ;; Github Explore
  "G" 'github-explorer
  ;; Quickrun
  "r" 'spring-run-code
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
  "R" 'ranger
  ;; Insert License
  "il" 'license-templates-insert
  ;; diff-hl
  "dn" 'diff-hl-next-hunk
  "dp" 'diff-hl-previous-hunk
  ;; outline
  "fs" 'outline-show-entry
  "fh" 'outline-hide-entry
  "fa" 'outline-show-all
  "fA" 'outline-hide-body
  ;; Treemacs
  "tt" 'treemacs
  "ts" 'treemacs-select-window
  ;; Avy
  "ac" 'avy-goto-char
  ;; Awesome-tab
  "at" 'awesome-fast-switch/body
  ;; FlyCheck
  "fy" 'flycheck-mode
  ;; Keypad mode
  "x" 'spring/evil-keypad-execute
  "e" 'spring/evil-keypad-execute
  "b" 'spring/evil-keypad-execute
  "q" 'spring/evil-keypad-execute
  ;; Other functions
  "mf" 'mark-defun
  "mh" 'mark-whole-buffer
  "fr" 'recentf-open-files
  "." 'xref-find-definitions
  "fk" 'describe-key
  "ft" 'spring/terlat-translate
  "fT" 'spring/terlat-translate-insert
  "fc" 'spring/test-color
  "fC" 'spring/input-char-number
  "fS" 'spring/set-variable-region
  "fv" 'spring/print-vars-value
  "zf" 'spring/format-commit
  "h" 'spring/hugo)

;;; Functions
(defun spring/evil-digit-argument (arg)
  "The digit argument function."
  (interactive (list (spring/arg-with-middle-keyboard)))
  (prefix-command-preserve-state)
  (setq prefix-arg arg)
  (universal-argument--mode))

(defun spring/evil-keypad-execute ()
  "Execute the keypad command."
  (interactive)
  (let ((key (pcase last-input-event
               (120 "C-x ") (101 "M-") (98 "C-M-") (113 "C-q ")))
        tmp)
    (if (null key)
        (message "[Evil]: '%s' prefix is not defined in keypad."
                 (char-to-string last-input-event))
      (message key)
      (while (not (eq 13
                      (setq tmp (read-char))))
        (setq key (concat key
                          (cond ((= tmp 44) "C-")
                                ((= tmp 46) "M-")
                                ((= tmp 47) "C-M-")
                                (t (concat (char-to-string tmp) " ")))))
        (message key))
      (setq key (substring key 0 -1))
      (if (commandp (setq tmp (key-binding (read-kbd-macro key))))
          (call-interactively tmp)
        (message "[Evil]: '%s' is not defined." key)))))

(provide 'init-evil)
