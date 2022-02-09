;;; This file is used for code running.

(defun spring-run-code-get-file-name (buffer-name suffix)
  "The function to get the file name without file suffix and return it."
  (let ((result (progn
                  (string-match (concat "^\\(.*\\)\\." suffix) buffer-name)
                  (match-string 1 buffer-name))))
    result))

(defun spring-run-code (&optional not-run)
  "The function to run code."
  (interactive "P")
  (catch 'run-out
    (let (file-name command unknow-mode)
      (when (buffer-modified-p)
        (save-buffer))
      (pcase major-mode
        ('c-mode
         (setq file-name (spring-run-code-get-file-name (buffer-name) "c"))
         (setq command (format "gcc --std=c11 %s -o /tmp/%s; /tmp/%s"
                               (buffer-name) file-name file-name)))
        ('c++-mode
         (setq file-name (spring-run-code-get-file-name (buffer-name) "cpp"))
         (setq command (format "g++ -std=c++20 %s -o /tmp/%s; /tmp/%s"
                               (buffer-name) file-name file-name)))
        ('python-mode
         (setq command (concat "python3 " (buffer-name))))
        ('go-mode
         (setq command (concat "go run " (buffer-name))))
        ('rustic-mode
         (call-interactively #'rustic-cargo-run)
         (throw 'run-out t))
        (_ (message "There're no running way for current filetype.")
           (setq unknow-mode t)))
      (unless unknow-mode
        (if (get-buffer-window "*eshell*")
            (select-window (get-buffer-window "*eshell*"))
          (split-window nil nil 'above)
          (eshell))
        (insert command)
        (unless not-run
          (eshell-send-input))))))

(provide 'run-code)
