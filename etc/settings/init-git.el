;;;; This is the git config file for my configuration.

;;; Packages
;;; magit
(gpack magit
  :key (("C-' m" . magit-status)
        (magit-process-mode-map . ("x" . magit-process-kill)))
  :var (magit-display-buffer-function . 'spring/magit-display-buffer)
  :config
  (progn
    (transient-define-prefix magit-commit ()
     "Create a new commit or replace an existing commit."
     :info-manual "(magit)Initiating a Commit"
     :man-page "git-commit"
     ["Arguments"
      ("-a" "Stage all modified and deleted files"   ("-a" "--all"))
      ("-e" "Allow empty commit"                     "--allow-empty")
      ("-v" "Show diff of changes to be committed"   ("-v" "--verbose"))
      ("-n" "Disable hooks"                          ("-n" "--no-verify"))
      ("-R" "Claim authorship and reset author date" "--reset-author")
      (magit:--author :description "Override the author")
      (7 "-D" "Override the author date" "--date=" transient-read-date)
      ("-s" "Add Signed-off-by line"                 ("-s" "--signoff"))
      (5 magit:--gpg-sign)
      (magit-commit:--reuse-message)]
     [["Create"
       ("c" "Commit"         spring/format-commit)]
      ["Edit HEAD"
       ("e" "Extend"         magit-commit-extend)
       ("w" "Reword"         magit-commit-reword)
       ("a" "Amend"          magit-commit-amend)
       (6 "n" "Reshelve"     magit-commit-reshelve)]
      ["Edit"
       ("f" "Fixup"          magit-commit-fixup)
       ("s" "Squash"         magit-commit-squash)
       ("A" "Augment"        magit-commit-augment)
       (6 "x" "Absorb changes" magit-commit-autofixup)]
      [""
       ("F" "Instant fixup"  magit-commit-instant-fixup)
       ("S" "Instant squash" magit-commit-instant-squash)]]
     (interactive)
     (if-let ((buffer (magit-commit-message-buffer)))
         (switch-to-buffer buffer)
       (transient-setup 'magit-commit)))

    (defun spring/magit-get-magit-window ()
      "Get the magit buffer in current tab.
If the number of it is more than 1, return t.
If the number is equal to 1, return the window.
Otherwise return nil."
      (let ((current-window (selected-window))
            (windows 0)
            window)
        (when (derived-mode-p 'magit-section-mode)
          (setq windows 1
                window current-window))
        (other-window 1)
        (while (not (eq (selected-window) current-window))
          (when (derived-mode-p 'magit-section-mode)
            (setq windows (1+ windows)
                  window (selected-window)))
          (other-window 1))
        (cond ((= windows 1) window)
              ((> windows 1) t))))

    (defun spring/magit-display-buffer (buffer)
      "My own function to display magit buffer."
      (if (dired-file-preview--other-windows-p)
          (progn
            (let ((magit-window (spring/magit-get-magit-window)))
              (if magit-window
                  (progn
                    (when (windowp magit-window)
                      (when (eq magit-window (selected-window))
                        (other-window 1)))
                    (switch-to-buffer buffer)
                    (selected-window))
                (user-error "There's no magit window in current tab."))))
        (let ((current-window (selected-window))
              (window (split-window nil nil t)))
          (select-window window)
          (switch-to-buffer buffer)
          (select-window current-window)
          window)))))

;;; Git Sign
(gpack diff-hl
  :hook (after-init-hook . global-diff-hl-mode))

;;; Git Message
(gpack vc-msg
  :key ("C-' s" . vc-msg-show))

(provide 'init-git)
