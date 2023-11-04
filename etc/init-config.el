;;;; The main config file for my configuration.

(add-to-list 'load-path "~/.emacs.d/etc/tools")
(add-to-list 'load-path "~/.emacs.d/etc/settings")
(add-to-list 'load-path "~/.emacs.d/etc/languages")

;;; Init the empty lsp-mode.
(require 'lsp-mode)

;;; Main Settings
(require 'init-basic)
(require 'init-ui)
(require 'init-keymaps)
(require 'init-functions)
(require 'init-package)

;;; Languages
(require 'spring-c)
(require 'spring-python)
(require 'spring-go)
(require 'spring-js)
(require 'spring-dart)
(require 'spring-clojure)
(require 'spring-rust)

;;; Other settings
;;; Tools
(require 'emulting)
(spring/extra-add-to-list "~/.emacs.d/etc/tools/emulting.el")
(require 'run-code)
(require 'init-macros)
(setq sniem-macro-file (locate-library "init-macros"))

(let ((private-file "~/.emacs.d/etc/init-private.el"))
  (when (file-exists-p private-file)
    (load-file private-file)))

;;; Todo gets
(add-hook 'after-init-hook #'spring/todo-undo-p)
(add-hook 'after-init-hook #'spring/refresh-packages)

;;; Natively Compile
(add-hook 'after-init-hook (lambda ()
                             (dolist (file spring/extra-native-compile-items)
                               (spring/native-compile-or-load file))
                             (setq spring/extra-items-compiled t)))

(provide 'init-config)
