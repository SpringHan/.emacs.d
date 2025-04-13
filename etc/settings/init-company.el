;;;; This is the auto complete file for my configuration.

(use-package company
  :hook (emacs-lisp-mode . company-mode)
  :bind (:map company-active-map
              ("M-p" . nil)
              ("M-n" . nil)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-backends
        '((company-elisp company-dabbrev-code company-dabbrev company-keywords company-files company-capf))
        company-show-numbers t
        company-idle-delay 0.2
        company-echo-delay 0.2
        company-minimum-prefix-length 1)

  (use-package company-box
    :hook (company-mode . company-box-mode)))

(provide 'init-company)
