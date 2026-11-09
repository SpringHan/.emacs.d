;;; Telega  -*- lexical-binding: t; -*-

(use-package telega
  :init (setq telega-server-libs-prefix "/usr/")
  :commands (etaf-telega-open)

  :config
  (defun spring/kill-telega ()
    (interactive)
    (ignore-errors (etaf-telega-close))
    (ignore-errors (telega-kill))
    (ignore-errors (telega-server-kill))

    (when (get-buffer "*Telega Root*")
      (kill-buffer "*Telega Root*"))))

(provide 'init-telega)
