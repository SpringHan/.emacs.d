;;;; This file is the edit packages or other things for my configuration.

;;; Packages
;;; Snippet
(package-require yasnippet
  :key ("C-' C-y" . yas-expand-from-trigger-key)
  :hook (after-init-hook . yas-global-mode)
  :config (progn
            (package-require yasnippet-snippets)
            (setq yas-snippet-dirs '("~/.emacs.d/snippets"
                                     "~/.emacs.d/elpa/yasnippet-snippets-20200802.1658/snippets"))))

;;; isolate-mode
(package-require isolate
  :key (("C-' C-a s" . isolate-quick-add)
        ("C-' C-a S" . isolate-long-add)
        ("C-' C-a d" . isolate-quick-delete)
        ("C-' C-a D" . isolate-long-delete)
        ("C-' C-a c" . isolate-quick-change)
        ("C-' C-a C" . isolate-long-change)))

;;; paredit mode
(package-require paredit
  :hook ((lisp-mode-hook emacs-lisp-mode-hook eshell-mode-hook lisp-interaction-mode-hook) . paredit-mode)
  :key ("C-' f" . paredit-focus-on-defun))

;;; multiple cursor
(package-require multiple-cursors
  :key (("C-M-l" . mc/edit-lines)
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("M-m" . newline)))

;;; avy-mode
(package-require avy
  :key (("C-' a c" . avy-goto-char)
        ("C-' a C" . avy-goto-char-2)
        ("C-' a l" . avy-goto-line)
        ("C-' a w" . avy-goto-word-1)
        ("C-' a W" . avy-goto-word-0)
        ("C-' a r" . avy-resume)))

(provide 'init-edits)
