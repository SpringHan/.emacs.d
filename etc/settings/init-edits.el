;;;; This file is the edit packages or other things for my configuration.

;;; Packages
;;; Snippet
(use-package yasnippet
  :bind ("C-' C-y" . yas-expand-from-trigger-key)
  :hook (after-init-hook . yas-global-mode)
  :config
  (use-package yasnippet-snippets)
  (setq yas-snippet-dirs
        `("~/.emacs.d/snippets"
          ,(concat (file-name-directory (locate-library "yasnippet-snippets"))
                   "snippets"))))

;;; paredit mode
(use-package paredit
  :bind ("C-' f" . paredit-focus-on-defun)
  :hook
  ((lisp-mode emacs-lisp-mode eshell-mode lisp-interaction-mode clojure-mode) . paredit-mode)
  (paredit-mode . electric-indent-local-mode))

;;; Evil nerd commenter
(use-package evil-nerd-commenter)

(provide 'init-edits)
