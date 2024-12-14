;;; This file is used for python configuration

;;; Indent
;; (setq-default python-indent-offset 2)

;;; Completing
(gpack python-mode
  :hook (python-mode-hook . electric-indent-local-mode)
  :config (add-hook 'python-mode-hook (lambda () (treesit-parser-create 'python))))

; (gpack live-py-mode)

(provide 'spring-python)
