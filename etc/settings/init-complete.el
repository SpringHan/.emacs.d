;;;; This is the auto complete file for my configuration.

;;; Packages
;;; Company (Complete Anything)
(gpack company-c-headers)
(gpack company
  :hook (after-init-hook . global-company-mode)
  :config (progn
            (setq company-idle-delay 0
                  company-minimum-prefix-length 1)
            (with-eval-after-load
                'company
              (define-key company-active-map (kbd "M-p") nil)
              (define-key company-active-map (kbd "M-n") nil)
              (define-key company-active-map (kbd "C-n") #'company-select-next)
              (define-key company-active-map (kbd "C-p") #'company-select-previous))
            (push 'company-capf company-backends))
  :key ("C-' i" . company-yasnippet))
(gpack company-box
  :hook (company-mode-hook . company-box-mode))

(provide 'init-complete)
