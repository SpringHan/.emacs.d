;;; This file is used for code running.

(defgroup spring-code-running nil
  "My code running group."
  :group 'applications)

(defvar spring-code-running-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'spring-code-running-quit)
    (define-key map "r" 'spring-code-running-again)
    map)
  "The map of `spring-code-running-mode'.")

(defconst spring-code-running-buffer "*Run-Code*"
  "The buffer of code running.")

(defcustom spring-code-running-last-command ""
  "The command of last running."
  :type 'string
  :group 'spring-code-running)

(define-derived-mode spring-code-running-mode nil "Code-Running"
  "The Code Running for my Emacs."
  :syntax-table nil
  :abbrev-table nil
  :group 'spring-code-running)

(defun spring-code-running-quit ()
  "Quit the code running."
  (interactive)
  (setq spring-code-running-last-command "")
  (kill-buffer-and-window))

(defun spring-code-running-again ()
  "Repeat the last command."
  (interactive)
  (shell-command spring-code-running-last-command spring-code-running-buffer))

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
      (switch-to-buffer spring-code-running-buffer)
      (spring-code-running-mode)
      (when (featurep 'evil)
        (evil-change-state 'emacs))
      (shell-command command spring-code-running-buffer)
      (setq spring-code-running-last-command command))))

(provide 'run-code)
