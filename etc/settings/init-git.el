;;;; This is the git config file for my configuration.

;;; Packages
;;; magit
(package-require magit
	:key ("C-' m" . magit-status))

;;; LightGit
(package-require lightgit
	:load-path "~/.emacs.d/third-party/lightgit")

;;; Git Sign
(package-require diff-hl
	:hook (after-init-hook . global-diff-hl-mode))

;;; Git Message
(package-require vc-msg
	:key ("C-' s" . vc-msg-show))

(provide 'init-git)
