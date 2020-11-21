;;;; This is the git config file for my configuration.

;;; Packages
;;; magit
(gpack magit
  :key ("C-' m" . magit-status))

;;; LightGit
(gpack lightgit
  :load-path "~/.emacs.d/third-party/lightgit")

;;; Git Sign
(gpack diff-hl
  :hook (after-init-hook . global-diff-hl-mode))

;;; Git Message
(gpack vc-msg
  :key ("C-' s" . vc-msg-show))

(provide 'init-git)
