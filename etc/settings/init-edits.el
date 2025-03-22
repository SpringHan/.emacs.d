;;;; This file is the edit packages or other things for my configuration.

;;; Packages
;;; Snippet
(gpack yasnippet
  :key ("C-' C-y" . yas-expand-from-trigger-key)
  :hook (after-init-hook . yas-global-mode)
  :config (progn
            (gpack yasnippet-snippets)
            (gpack license-snippets
              :un-require)
            (setq yas-snippet-dirs
                  `("~/.emacs.d/snippets"
                    ,(concat (file-name-directory (locate-library "yasnippet-snippets"))
                             "snippets")))))

;;; paredit mode
(gpack paredit
  :hook (((lisp-mode-hook emacs-lisp-mode-hook eshell-mode-hook lisp-interaction-mode-hook clojure-mode-hook) . paredit-mode)
         (paredit-mode-hook . electric-indent-local-mode))
  :key ("C-' f" . paredit-focus-on-defun))

;;; Evil nerd commenter
(gpack evil-nerd-commenter)

(provide 'init-edits)
