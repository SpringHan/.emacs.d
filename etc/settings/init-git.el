;;;; This is the git config file for my configuration.

;;; Packages
;;; magit
(gpack magit
  :key (("C-' m" . magit-status)
        (magit-process-mode-map . ("x" . magit-process-kill))
        (magit-mode-map . ("q" . spring/kill-magit)))
  :var (magit-display-buffer-function . 'spring/magit-display-buffer)
  :config
  (defun spring/magit-display-buffer (buffer)
    "My own function to display magit buffer."
    (let ((current-window (selected-window))
          window)
      (if (dired-file-preview--other-windows-p)
          (progn
            (other-window 1)
            (setq window (selected-window)))
        (setq window (split-window nil nil t)))
      (select-window window)
      (switch-to-buffer buffer)
      (select-window current-window)
      window)))

;;; Git Sign
(gpack diff-hl
  :hook (after-init-hook . global-diff-hl-mode))

;;; Git Message
(gpack vc-msg
  :key ("C-' s" . vc-msg-show))

(provide 'init-git)
