;;;; This file is used for python configuration

;;; Indent
(setq-default python-indent-offset 2)

;;; Completing
(gpack lsp-pyright
  :hook (python-mode-hook . (lambda () (require 'lsp-pyright) (lsp))))

;; (gpack lsp-python-ms
;;   :config (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode-hook . (lambda ()
;;                               (require 'lsp-python-ms)
;;                               (lsp))))

(gpack elpy
  :config (with-eval-after-load 'python
            (unless (or (not buffer-file-name)
                        (string= (file-name-extension buffer-file-name) "org"))
              (elpy-enable))))

(provide 'spring-python)
