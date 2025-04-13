;;;; This file is used for packages configuration and more
;;; Set the plugin keybinding
(define-prefix-command 'plugin-key)
(global-set-key (kbd "C-'") 'plugin-key)

;;; Package manager
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(defun use-package-require (name &optional no-require body)
  (if use-package-expand-minimally
      (use-package-concat
       (unless no-require
         (list (use-package-load-name name)))
       body)
    (if no-require
        body
      (use-package-with-elapsed-timer
          (format "Loading package %s" name)
        `((if (not ,(use-package-load-name name))
              (display-warning 'use-package
                               (format "Cannot load %s" ',name)
                               :error)
            ,@body))))))
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

;;; Packages
(require 'init-company)
(require 'init-edits)
(require 'init-lsp)
(require 'init-tools)
(require 'init-awesome-tray)
(require 'init-sniem)
(require 'init-eee)
(require 'init-aider)
(require 'init-git)
(require 'init-modes)
(require 'init-treesit)
(require 'init-telega)
(require 'init-org)

(provide 'init-package)
