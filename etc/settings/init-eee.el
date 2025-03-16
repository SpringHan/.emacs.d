;;; Eee.el

(gpack eee
  :repo "eval-exec/eee.el"
  :var (ee-terminal-command . "kitty")
  :key (("C-' a" . ee-rg)
        ("C-' l" . ee-lazygit)
        ("C-' h" . ee-hire)))

(defun ee-hire ()
  (interactive)
  (ee-message "Hire")
  (ee-start-process-shell-command-in-terminal
   "ee-hire"
   (format "%s %s"
           (expand-file-name "~/.emacs.d/scripts/hire.sh")
           default-directory)
   (lambda(process)
     (funcall #'ee-jump-from "/tmp/hire.tmp"))))

(provide 'init-eee)
