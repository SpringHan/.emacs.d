;;;; This is the git config file for my configuration.

;;; Packages
;;; magit
(package-require 'magit
	:keymap '(("C-' m" . magit-status)))

;;; LightGit
(package-require 'lightgit
	:outside
	:before-load-eval '(add-to-list 'load-path "~/.emacs.d/third-party/lightgit"))

;;; Git Sign
(package-require 'diff-hl
	:hook '(after-init-hook . global-diff-hl-mode))

;;; Git Message
(package-require 'vc-msg
	:keymap '(("C-' s" . vc-msg-show)))

(provide 'init-git)
