;;; Dsh emacs  -*- lexical-binding: t; -*-

;;; Deepseek Harness
(use-package dsh-emacs
  :init
  (git-download-ensure "dsh-emacs" "SpringHan/dsh-emacs")
  :load-path "~/.emacs.d/third-party/dsh-emacs"
  :bind (("C-q C-d" . dsh-emacs)
         ("C-q C-s" . dsh-emacs-server-start)
         ("C-q C-q" . dsh-emacs-server-stop)
         ("C-q C-h" . dsh-emacs-plugin-routes)
         ("C-q C-i" . dsh-emacs-plugin-insert-route-call)
         ("C-q C-w" . spring/dsh-emacs-get-cost))
  :config
  (setq dsh-emacs-server-auto-start nil))

(defun spring/dsh-emacs-get-cost ()
  (interactive)
  (let ((result (dsh-emacs-plugin-routes "dsh-whale" "GET" "/dsh-whale/balance.json")))
    (if (not (alist-get 'ok result))
        (user-error "[Dsh]: Failed to get current cost!")
      (message "[Dsh]: %.2f, today cost: %.2f"
               (alist-get 'totalBalance result)
               (alist-get 'todayUsage result)))))

(provide 'init-dsh)
