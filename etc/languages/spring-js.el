;;;; The file for js.

;;; js2-mode
(use-package js2-mode
  :disabled
  :config
  (setq-default js-indent-level 2)
  (setq auto-mode-alist
        (append
         '(("\\.js\\'" . js2-mode))
         auto-mode-alist)))

(provide 'spring-js)
