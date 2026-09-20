;;; Eee.el

(use-package eee
  :init (git-download-ensure "eee.el" "SpringHan/eee.el" 1)
  :load-path "~/.emacs.d/third-party/eee.el"
  :bind (("C-' a" . ee-rg)
         ("C-' l" . ee-lazygit)
         ("C-' h" . ee-hire)
         ("C-' '" . spring/output-path))

  :config
  (setq ee-terminal-command "kitty")
  (defun ee-hire ()
    (interactive)
    (catch 'stop
      (when (file-exists-p "/tmp/hire.tmp")
        (let ((path (with-temp-buffer
                      (insert-file-contents "/tmp/hire.tmp")
                      (buffer-string))))
          (unless (string-empty-p path)
            (ee-jump-from "/tmp/hire.tmp")
            (with-temp-file "/tmp/hire.tmp")
            (throw 'stop nil))))

      (ee-message "Hire")
      (ee-start-process-shell-command-in-terminal
       "ee-hire"
       (format "%s %s"
               (expand-file-name "~/.emacs.d/scripts/hire.sh")
               default-directory)
       (lambda (process)
         (funcall #'ee-jump-from "/tmp/hire.tmp")
         (with-temp-file "/tmp/hire.tmp")))))

  (defun spring/output-path ()
    "Output `default-directory' to '/tmp/hire.tmp'."
    (interactive)
    (let ((temp-file "/tmp/hire.tmp"))
      (unless (file-exists-p temp-file)
        (make-empty-file temp-file))

      (with-temp-file "/tmp/hire.tmp"
        (erase-buffer)
        (insert default-directory)))))

(provide 'init-eee)
