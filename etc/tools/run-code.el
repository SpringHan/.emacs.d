;;; This file is used for code running.

(defun spring-run-code-get-file-name (buffer-name suffix)
  "The function to get the file name without file suffix and return it."
  (let ((result (car (split-string buffer-name suffix t))))
    result))

(defun spring-run-code ()
  "The function to run code."
  (interactive)
  (let ((modes '(c-mode python-mode))
        file-name command)
    (if (not (memq major-mode modes))
        (message "There're no running way for current filetype.")
      (pcase major-mode
        ('c-mode
         (setq file-name (spring-run-code-get-file-name (buffer-name) ".c"))
         (setq command (format "gcc --std=c11 %s -o /tmp/%s; /tmp/%s"
                               (buffer-name) file-name file-name)))
        ('python-mode
         (setq command (concat "python3 " (buffer-name))))
        ('go-mode
         (setq command (concat "go run " (buffer-name)))))
      (split-window nil nil 'above)
      (eshell)
      (insert command)
      (eshell-send-input))))

(provide 'run-code)
