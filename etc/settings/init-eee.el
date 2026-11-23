;;; Eee.el  -*- lexical-binding: t; -*-

(use-package eee
  :init (git-download-ensure "eee.el" "SpringHan/eee.el" 1)
  :load-path "~/.emacs.d/third-party/eee.el"
  :bind (("C-' a" . ee-rg)
         ("C-' l" . ee-lazygit)
         ("C-' '" . spring/output-path))

  :config
  (setq ee-terminal-command "kitty")
  (defun ee-find-file ()
    (interactive)
    (if (display-graphic-p)
        (call-process-shell-command
         (format
          "niri-scratchpad target --spawn \"kitty --title scratch_emacs_hire hire --start-from %s --quit-after-output\" --as-float title scratch_emacs_hire"
          default-directory)
         nil
         0)
      (emulting '(file new-file))))

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
