;;; Aider

(defun spring/get-aider-api ()
  (with-temp-buffer
    (insert-file-contents
     (locate-user-emacs-file "aider-key"))
    (string-trim-right (buffer-string))))

(gpack aidermacs
  :repo "SpringHan/aidermacs"
  :key ("C-' A" . aidermacs-transient-menu)
  :var ((aidermacs-default-model . "deepseek"))
  :config (setenv "DEEPSEEK_API_KEY" (spring/get-aider-api)))
;; (spring/native-compile-or-load "~/.emacs.d/third-party/aidermacs/aidermacs" t t)

(provide 'init-aider)
