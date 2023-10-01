;;;; This file is used for packages configuration and more
;;; Set the plugin keybinding
(define-prefix-command 'plugin-key)
(global-set-key (kbd "C-'") 'plugin-key)
(global-set-key (kbd "C-' R") #'spring/reopen-file)


;;; Packages
(require 'init-complete)
(require 'init-edits)
(require 'init-lsp)
(require 'init-tools)
(require 'init-awesome-tray)
(require 'init-sniem)
(require 'init-git)
(require 'init-modes)
(require 'init-org)

;;;; Themes
(load-the-theme)
(package-initialize)

(provide 'init-package)
