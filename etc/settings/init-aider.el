;;; Aider

(use-package aidermacs
  :init
  (git-download-ensure "aidermacs" "SpringHan/aidermacs" 1)
  (setq aidermacs-default-model "deepseek")
  (defun spring/get-aider-api ()
    (with-temp-buffer
      (insert-file-contents
       (locate-user-emacs-file "aider-key"))
      (string-trim-right (buffer-string))))

  :load-path "~/.emacs.d/third-party/aidermacs"
  :bind ("C-' A" . aidermacs-transient-menu)
  :config
  (setenv "DEEPSEEK_API_KEY" (spring/get-aider-api)))

(spring/extra-add-to-list "~/.emacs.d/third-party/aidermacs/aidermacs")
;; (spring/native-compile-or-load "~/.emacs.d/third-party/aidermacs/aidermacs" t t)

(provide 'init-aider)
