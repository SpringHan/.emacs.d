;;;; This is the git config file for my configuration.

;;; Packages
;;; magit
(gpack magit
  :key (("C-' m" . magit-status)
        (magit-process-mode-map . ("x" . magit-process-kill))
        (magit-mode-map . ("q" . spring/kill-magit)))
  :var (magit-display-buffer-function . 'spring/magit-display-buffer)
  :config
  (progn
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
        window))

    (transient-define-prefix magit-tag ()
      "Create or delete a tag."
      :man-page "git-tag"
      ["Arguments"
       ("-f" "Force"    ("-f" "--force"))
       ("-a" "Annotate" ("-a" "--annotate"))
       ("-s" "Sign"     ("-s" "--sign"))
       (magit-tag:--local-user)]
      [["Create"
        ("t"  "tag"     magit-tag-create)
        ("r"  "release" magit-tag-release)]
       ["Do"
        ("k"  "delete"  magit-tag-delete)
        ("p"  "prune"   magit-tag-prune)
        ("d"  "delete remotely" spring/magit-delete-remote-tag)]])))

(defun spring/magit-delete-remote-tag (tag-name remote)
  "Delete tag with its TAG-NAME in REMOTE."
  (interactive (list (read-string "Enter the tag's name: ")
                     (magit-read-remote "Enter the remote: ")))
  (magit--shell-command (format "git push --delete %s %s"
                                remote tag-name)))

;;; Git Sign
(gpack diff-hl
  :hook (after-init-hook . global-diff-hl-mode))

;;; Git Message
(gpack vc-msg
  :key ("C-' s" . vc-msg-show))

(provide 'init-git)
