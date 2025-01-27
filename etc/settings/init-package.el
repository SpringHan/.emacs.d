;;;; This file is used for packages configuration and more
;;; Set the plugin keybinding
(define-prefix-command 'plugin-key)
(global-set-key (kbd "C-'") 'plugin-key)


;;; Packages
(require 'init-complete)
(require 'init-edits)
(require 'init-lsp)
(require 'init-tools)
(require 'init-awesome-tray)
(require 'init-sniem)
(require 'init-git)
(require 'init-modes)
(require 'init-treesit)
(require 'init-telega)
(require 'init-org)
(require 'init-aider)

(package-initialize)

(provide 'init-package)
