;;;; This is the auto complete file for my configuration.

;;; Packages
;;; Company (Complete Anything)
(gpack company-c-headers)

(gpack company
  :hook ((after-init-hook . global-company-mode)
         (emacs-lisp-mode-hook . (lambda ()
                                   (setq-local company-backends
                                               (append '(company-elisp)
                                                       company-backends)))))
  :var ((company-show-numbers . t)
        (company-idle-delay . 0)
        (company-echo-delay . 0))
  :config (progn
            (setq company-idle-delay 0
                  company-minimum-prefix-length 1)
            (with-eval-after-load
                'company
              (define-key company-active-map (kbd "M-p") nil)
              (define-key company-active-map (kbd "M-n") nil)
              (define-key company-active-map (kbd "C-n") #'company-select-next)
              (define-key company-active-map (kbd "C-p") #'company-select-previous)
              (setq company-backends
                    '((company-dabbrev-code company-dabbrev company-keywords company-files company-capf))))))

(gpack company-box
  :hook (company-mode-hook . company-box-mode))

(gpack company-web
  :hook (web-mode-hook . (lambda ()
                           (setq-local company-backends
                                       (append '(company-web-html company-web-jade company-web-slim)
                                               company-backends)))))

;; (gpack company-tabnine
;;   :hook ((python-mode-hook go-mode-hook) . (lambda ()
;;                                              (setq-local company-backends
;;                                                          (append '(company-tabnine) company-backends)))))

(provide 'init-complete)
