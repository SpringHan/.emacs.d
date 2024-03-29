;;;; This is the basic settings for my configuration.

(fset 'yes-or-no-p 'y-or-n-p) ; Change the asking's answer way
(delete-selection-mode t) ; Delete the seleceted text
(show-paren-mode t) ; Highlight the ()
(electric-pair-mode t) ; Auto complete the ()
(setq electric-pair-pairs
      '((?\" . ?\")
        (?\( . ?\))
        (?\< . ?\>)
        (?\{ . ?\}))) ; Set the electric-pair-mode's pair keywords
(setq make-backup-files nil ; Don't let Emacs make up backup file
      create-lockfiles nil ;Don't make lockfile
      auto-save-default nil ; Don't auto save the file
      )
(setq initial-scratch-message
      ";; Spring Emacs
;; This is the scratch buffer for you.\n\n")
(setq-default tab-width 2) ; The tab width
(setq-default indent-tabs-mode nil) ; Use tab indent
(setq-default css-indent-offset 2) ; Set css indent width
(column-number-mode t) ; Show the column number in the modeline
(setq backward-delete-char-untabify-method nil) ; Delete the tab by once
(setq user-emacs-directory "~/.emacs.d/var/")
(setq user-init-file "~/.emacs.d/var/user-init.el")
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

(add-hook 'markdown-mode-hook #'(lambda ()
                                  (define-key markdown-mode-map (kbd "C-c C-c TAB") 'markdown-table-align))) ; Add the markdown table align keymap
(add-hook 'erc-mode-hook #'(lambda () (linum-mode -1))) ; Close the line number in the erc mode
(add-hook 'shell-mode-hook #'(lambda () (company-mode -1) (define-key shell-mode-map (kbd "C-c l") 'spring/shell-clear)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (outline-minor-mode t)))
(add-hook 'web-mode-hook #'(lambda () (outline-minor-mode t)))
;;; Calendar
(defvar spring/calendar-disable-modeline-timer nil
  "The timer used to disable modeline.")
(advice-add 'calendar-exit
            :after
            (lambda (&optional kill)
              "Disable modeline."
              (spring/disable-modeline)))
(add-hook 'calendar-mode-hook #'spring/enable-modeline)
(add-hook 'calendar-mode-hook (lambda ()
                                (setq spring/calendar-disable-modeline-timer
                                      (run-with-timer
                                       10 5
                                       (lambda ()
                                         (unless (get-buffer "*Calendar*")
                                           (spring/disable-modeline)))))))

(provide 'init-basic)
