;;; Aider

(gpack aidermacs
  :key ("C-' A" . aidermacs-transient-menu)
  :var ((aidermacs-default-model . "deepseek"))
  :config (setenv "DEEPSEEK_API_KEY" (spring/get-aider-api)))

(defun spring/get-aider-api ()
  (with-temp-buffer
    (insert-file-contents
     (locate-user-emacs-file "aider-key"))
    (string-trim-right (buffer-string))))

(provide 'init-aider)
