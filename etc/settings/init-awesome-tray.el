;;;; This is the awesome-tray settings for my emacs configuration.


;;; awesome-tray
(package-require
 'awesome-tray
 :before-load-eval '(add-to-list 'load-path "~/.emacs.d/third-party/awesome-tray")
 :hook '(after-init-hook . awesome-tray-mode))

(defun awesome-tray-read-only ()
	(if (eq buffer-read-only t)
			"read-only"
		""))

(defun awesome-tray-buffer-modified ()
	(if (buffer-modified-p)
			"*"
		""))

(add-to-list 'awesome-tray-module-alist '("buffer-read-only" . (awesome-tray-read-only awesome-tray-module-parent-dir-face)))
(add-to-list 'awesome-tray-module-alist '("buffer-modified-p" . (awesome-tray-buffer-modified awesome-tray-module-date-face)))

(setq awesome-tray-active-modules '("evil" "location" "buffer-read-only" "buffer-modified-p" "buffer-name" "git" "parent-dir" "mode-name" "date"))

(provide 'init-awesome-tray)
