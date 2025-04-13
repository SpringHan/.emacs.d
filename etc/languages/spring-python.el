;;; This file is used for python configuration

;;; Indent
;; (setq-default python-indent-offset 2)

;;; Completing
(use-package python-mode
  :hook (python-mode . electric-indent-local-mode)
  :config (add-hook 'python-mode-hook (lambda () (treesit-parser-create 'python))))

; (gpack live-py-mode)

(provide 'spring-python)
