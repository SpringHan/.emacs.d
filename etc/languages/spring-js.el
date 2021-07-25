;;;; The file for js.

;;; js2-mode
(gpack js2-mode
  :disable
  :config (setq auto-mode-alist
                (append
                 '(("\\.js\\'" . js2-mode))
                 auto-mode-alist)))

(setq-default js-indent-level 2)

(provide 'spring-js)
