;;;; This is the auto complete file for my configuration.

(gpack company
  :hook (emacs-lisp-mode-hook . company-mode)
  :var ((company-show-numbers . t)
        (company-idle-delay . 0.2)
        (company-echo-delay . 0.2)
        (company-minimum-prefix-length . 1))
  :config (progn
            (with-eval-after-load
                'company
              (define-key company-active-map (kbd "M-p") nil)
              (define-key company-active-map (kbd "M-n") nil)
              (define-key company-active-map (kbd "C-n") #'company-select-next)
              (define-key company-active-map (kbd "C-p") #'company-select-previous)
              (setq company-backends
                    '((company-elisp company-dabbrev-code company-dabbrev company-keywords company-files company-capf))))))

(gpack company-box
  :hook (company-mode-hook . company-box-mode))

(provide 'init-company)
