;;;; The main config file for my configuration.

(add-to-list 'load-path "~/.emacs.d/etc/tools")
(add-to-list 'load-path "~/.emacs.d/etc/settings")
(add-to-list 'load-path "~/.emacs.d/etc/languages")

;;; Main Settings
(require 'init-basic)
(require 'init-ui)
(require 'init-keymaps)
(require 'init-functions)
(require 'init-package)
(require 'init-hugo)

;;; Languages
(require 'spring-c)
(require 'spring-python)
(require 'spring-go)
(require 'spring-js)
(require 'spring-dart)
(require 'spring-clojure)

;;; Other settings
;;; Tools
(require 'task-reminder)
(require 'emulting)
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
(native-compile-async "~/.emacs.d/etc/tools/emulting.el" 2 t)
(native-compile-async '("~/.emacs.d/third-party/sniem/"
                        "~/.emacs.d/third-party/netease-cloud-music.el")
                      5 t)

(provide 'init-config)
