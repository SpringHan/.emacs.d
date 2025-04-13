;;;; This is the git config file for my configuration.

;;; Git Sign
(use-package diff-hl
  :hook (after-init-hook . global-diff-hl-mode))

;;; Git Message
(use-package vc-msg
  :bind ("C-' s" . vc-msg-show))

(provide 'init-git)
