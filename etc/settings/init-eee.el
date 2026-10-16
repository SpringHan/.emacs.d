;;; Eee.el  -*- lexical-binding: t; -*-

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
    (ee-message "Hire")
    (ee-start-process-shell-command-in-terminal
     "ee-hire"
     (format "%s %s"
             (expand-file-name "~/.emacs.d/scripts/hire.sh")
             default-directory)
     nil))

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
