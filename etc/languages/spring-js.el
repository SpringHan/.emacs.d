;;;; The file for js.

;;; js2-mode
(gpack js2-mode
  :disable
  :config (setq auto-mode-alist
                (append
                 '(("\\.js\\'" . js2-mode))
                 auto-mode-alist)))

(provide 'spring-js)
