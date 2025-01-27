;;; Aider

(gpack aider
  :repo "tninja/aider.el"
  :key ("C-' a" . aider-transient-menu)
  :config (progn
            (setq aider-args '("--no-auto-commits" "--model" "openrouter/deepseek/deepseek-coder"))
            (setenv "OPENROUTER_API_KEY" (with-temp-buffer
                                           (insert-file-contents "~/.config/openrouter/key.txt")
                                           (string-trim (buffer-string))))))

(provide 'init-aider)
